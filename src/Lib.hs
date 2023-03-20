module Lib where

import Types
import Data.List (intercalate, elemIndex, transpose)
import Data.Char (toUpper, intToDigit)
import Data.Time (UTCTime)
import Text.XML.HXT.Core (a_name, intToHexString)
import Data.Either (fromRight)
import Control.Monad (forM_)

-- Add a SendRequest to an account, given a requestor, recipient, amount, account, and requestedDateTime.
addSpendRequestTx :: Vk -> Vk -> Int -> Account -> UTCTime -> Either RequestException Account
addSpendRequestTx requestor recipient amt (Account id signers bal threshhold spendRequests) utcTime 
    | amt < 0 
        = Left OtherEx
    | notElem requestor _TEST_Vks_ || notElem recipient _TEST_Vks_
        = Left OtherEx
    | otherwise
        = Right $ Account id signers bal threshhold $ spendRequests ++ [SpendRequestTx {
                        stx_base=AccountRequestTxBase {
                            btx_txState=TxRequested
                            , btx_txId="" -- TODO unused - remove
                            , btx_txCreator=requestor
                            , btx_endorseTxs=[]
                            , btx_createdDateTime=utcTime
                            , btx_accountId=id}
                        , stx_recipient=recipient
                        , stx_spendAmount=amt
                        }]

-- functions to help UI listings.  Txs awaiting your endorsement
getPendingTxsForVk :: ()
getPendingTxsForVk = undefined

-- Check if the Vk and Sk pair is authentic.  A genuine implementation would instead use cryptographic signing and verification
isAuthenticatedPair :: Vk -> Sk -> Bool
-- For now, just see if the vk and sk pair is in the test set
-- TODO confirm these are a matching pair, not just both in the test lists. Will require some refactoring
isAuthenticatedPair vk sk = elem vk _TEST_Vks_ && elem sk _TEST_Sks_ 

replaceAccount :: Wallet -> Account -> Either WalletException Wallet
replaceAccount w ra =
    let 
        oldAccounts2 = ah_accounts w
        oldAccounts = ah_accounts w
        newAccounts = map (\account -> if a_accountId ra == a_accountId account then ra else account) oldAccounts
    in
    -- TODO implement verification replacement worked. For now, assuming it did
    let newWallet = Wallet newAccounts (ah_activeAccountIndex w) (ah_authenticatedVk w) in
        Right newWallet

prettyWallet :: Wallet -> String
prettyWallet w = prettyAccountsWithNum $ ah_accounts w

prettyAccountsWithNum :: [Account] -> String
prettyAccountsWithNum accounts =
            let numberedAccounts = zip [0,1..] accounts in
            -- unlines $ fmap (\(i, a) -> " #" ++ show i ++ "\n" ++ prettyAccount a ) numberedAccounts
            unlines $ fmap (\(i, a) -> " #" ++ show i ++ " " ++ a_accountId a ) numberedAccounts

prettyAccount :: Account -> String
prettyAccount a =
    unlines [
        " Account Id: " ++ a_accountId a
        , " Signers: " ++ intercalate ", " (a_signers a)
        , " Required Endorsers: " ++ show (a_requiredSigs a)
        , " Balance: " ++ show (a_balance a)
        , " Spend Requests: \n" ++ prettyRequests (a_spendTxs a)
    ]

prettyRequests :: [SpendRequestTx] -> String
prettyRequests [] = "(no Spend Requests)"
prettyRequests requests =
    intercalate "\n" $ map prettyRequestWithNum $ zip [0, 1..] requests

prettyRequestWithNum :: (Int, SpendRequestTx) -> String
prettyRequestWithNum (i, request) = unlines [
        "  #" ++ [intToDigit i]
        , prettyRequest request
        ]

prettyRequest :: SpendRequestTx -> String
prettyRequest request = unlines [
            "    Recipient: " ++ show (stx_recipient request)
            , "    Amount: " ++ show (stx_spendAmount request)
            , "    Requester: " ++ show (btx_txCreator base)
            , "    Requested at: " ++ show (btx_createdDateTime base)
            , "    Workflow State: " ++ show (btx_txState base)
            , "    Additional Endorsers: " ++ prettyEndorsers (btx_endorseTxs base)
            ]
            where base = stx_base request

prettyEndorsers :: [AccountTxVoteTx] -> String
prettyEndorsers [] = "(none)"
prettyEndorsers endorsers = intercalate ", " $ map prettyEndorser endorsers

prettyEndorser :: AccountTxVoteTx -> String
prettyEndorser endorser = show (atxv_approverVk endorser)

-- Find the index of a SendRequestTx in a list, based on its dateTime. This is a admittedly weak identifier that should be improved in the future.
findSendRequestIndex :: [SpendRequestTx] -> SpendRequestTx -> Maybe Int
findSendRequestIndex [] _  = Nothing
findSendRequestIndex sendRequests newSendRequest =
    fsr sendRequests newSendRequest (length sendRequests - 1)
    where
        -- fsr :: [SpendRequestTx] -> SpendRequestTx -> Int -> Maybe Int
        fsr [] _ _          = Nothing
        fsr (sr:srs) a idx  = if btx_createdDateTime (stx_base sr) == btx_createdDateTime (stx_base newSendRequest)
            then
                Just idx
            else
                fsr srs a (idx - 1)

-- For the supplied account, sendRequest, and spendEndorsement, update the account and spendRequest, and return them
applyEndorsement :: Account -> SpendRequestTx -> AccountTxVoteTx -> Either RequestException (Account, SpendRequestTx)
applyEndorsement a sendRequest endorsement = 
    let mFoundIndex = findSendRequestIndex (a_spendTxs a) sendRequest in
    case mFoundIndex of
        Nothing -> Left EndorsementTargetNotFoundEx
        Just index -> 
            -- verify old spendRequest is still in a pending state that can accpet endorsements
            case btx_txState $ stx_base $ a_spendTxs a !! index of
                TxApproved    -> Left AlreadyFinalizedEx
                TxRejected    -> Left UnauthorizedSignerEx
                TxExpired     -> Left TimedOutEx
                TxApprovedNsf -> Left NsfEx
                TxOtherError  -> Left RedundantVoteEx
                _             -> 
                    let
                        eUpdatedSendRequest = applyEndorsementToSpendTx sendRequest (a_balance a) endorsement 
                    in
                    case eUpdatedSendRequest of
                        Left ex -> Left ex
                        Right updatedSendRequest -> 
                            Right (Account {
                                    a_spendTxs = newSpendTxs
                                    , a_signers= a_signers a
                                    , a_requiredSigs = a_requiredSigs a
                                    , a_balance= newBalance
                                    , a_accountId= a_accountId a
                                    } 
                                    , fst updatedSendRequest)
                                where
                                    newSpendTxs = (\oldSendRequest -> 
                                            if btx_createdDateTime (stx_base oldSendRequest) == btx_createdDateTime (stx_base sendRequest) 
                                                then fst updatedSendRequest
                                                else oldSendRequest
                                            ) <$> a_spendTxs a
                                    -- compute the new SpendRequest state
                                    -- if approved
                                    -- TODO EE! issue with newBalance!  Check for NSF
                                    newBalance = 99999

-- For the supplied SpendRequest and account balance, apply the endorsement, compute and return the new request state and account balance
applyEndorsementToSpendTx :: SpendRequestTx -> Int -> AccountTxVoteTx -> Either RequestException (SpendRequestTx, Int)
applyEndorsementToSpendTx sendRequest oldAccountBalance endorsement = 
    case btx_txState (stx_base sendRequest) of
        TxRequested -> Left AlreadyFinalizedEx
        TxPending   -> Left AlreadyFinalizedEx
        _           -> if isApproved 
            then if stx_spendAmount sendRequest >= oldAccountBalance
                    then Right (updatedSpendRequest, oldAccountBalance - stx_spendAmount sendRequest ) -- TODO EE!! verify
                    else Left NsfEx
            else Right (updatedSpendRequest, oldAccountBalance)
            where 
                isApproved = True -- TODO EE! fix
                updatedSpendRequest = SpendRequestTx {
                    stx_spendAmount= stx_spendAmount sendRequest
                    , stx_recipient= stx_recipient sendRequest
                    , stx_base=AccountRequestTxBase {
                        btx_txState= btx_txState base -- TODO EE! update
                        , btx_txId= btx_txId base
                        , btx_txCreator= btx_txCreator base
                        , btx_endorseTxs= btx_endorseTxs base <> [endorsement]
                        , btx_createdDateTime= btx_createdDateTime base
                        , btx_accountId= btx_accountId base
                        }
                    } 
                    where base = stx_base sendRequest
                