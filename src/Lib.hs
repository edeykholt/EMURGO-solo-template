module Lib where

import Types
import Data.List (intercalate, elemIndex, transpose)
import Data.Char (toUpper, intToDigit)
import Data.Time (UTCTime)
import Text.XML.HXT.Core (a_name, intToHexString)
import Data.Either (fromRight)
import Control.Monad (forM_)

-- Add a SendRequest to an account, given a requestor, recipient, amount, account, and requestedDateTime.
addSendRequestTx :: Vk -> Vk -> Int -> Account -> UTCTime -> Either RequestException Account
addSendRequestTx requestor recipient amt (Account id signers bal threshhold sendRequests) utcTime 
    | amt < 0 
        = Left OtherEx
    | notElem requestor _TEST_Vks_ || notElem recipient _TEST_Vks_
        = Left OtherEx
    | otherwise
        = Right $ Account id signers bal threshhold $ sendRequests ++ [SendRequestTx {
                        stx_base=AccountRequestTxBase {
                            btx_txState=TxPendingEndorsement
                            , btx_txId="" -- TODO unused - remove
                            , btx_txCreator=requestor
                            , btx_endorsementTxs=[]
                            , btx_createdDateTime=utcTime
                            , btx_accountId=id}
                        , stx_recipient=recipient
                        , stx_sendAmount=amt
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
        , " Send Requests: \n" ++ prettyRequests (a_sendTxs a)
    ]

prettyRequests :: [SendRequestTx] -> String
prettyRequests [] = "(no Send Requests)"
prettyRequests requests =
    intercalate "\n" $ map prettyRequestWithNum $ zip [0, 1..] requests

prettyRequestWithNum :: (Int, SendRequestTx) -> String
prettyRequestWithNum (i, request) = unlines [
        "  #" ++ [intToDigit i]
        , prettyRequest request
        ]

prettyRequest :: SendRequestTx -> String
prettyRequest request = unlines [
            "    Recipient: " ++ show (stx_recipient request)
            , "    Amount: " ++ show (stx_sendAmount request)
            , "    Requester: " ++ show (btx_txCreator base)
            , "    Requested at: " ++ show (btx_createdDateTime base)
            , "    Workflow State: " ++ show (btx_txState base)
            , "    Additional Endorsers: " ++ prettyEndorsers (btx_endorsementTxs base)
            ]
            where base = stx_base request

prettyEndorsers :: [EndorsementTx] -> String
prettyEndorsers [] = "(none)"
prettyEndorsers endorsers = intercalate ", " $ map prettyEndorser endorsers

prettyEndorser :: EndorsementTx -> String
prettyEndorser endorser = show (atxv_approverVk endorser)

-- Find the index of a SendRequestTx in a list, based on its dateTime. This is a admittedly weak identifier that should be improved in the future.
findSendRequestIndex :: [SendRequestTx] -> SendRequestTx -> Maybe Int
findSendRequestIndex [] _  = Nothing
findSendRequestIndex sendRequests newSendRequest =
    fsr sendRequests newSendRequest (length sendRequests - 1)
    where
        -- fsr :: [SendRequestTx] -> SendRequestTx -> Int -> Maybe Int
        fsr [] _ _          = Nothing
        fsr (sr:srs) a idx  = if btx_createdDateTime (stx_base sr) == btx_createdDateTime (stx_base newSendRequest)
            then
                Just idx
            else
                fsr srs a (idx - 1)

-- For the supplied account, sendRequest, and Endorsement, update the account and sendRequest, and return them
applyEndorsement :: Account -> SendRequestTx -> EndorsementTx -> Either RequestException (Account, SendRequestTx)
applyEndorsement a sendRequest endorsement = 
    let mFoundIndex = findSendRequestIndex (a_sendTxs a) sendRequest in
    case mFoundIndex of
        Nothing -> Left EndorsementTargetNotFoundEx
        Just index -> 
            -- verify old sendRequest is still in a pending state that can accpet endorsements
            case btx_txState $ stx_base $ a_sendTxs a !! index of
                TxApproved    -> Left AlreadyFinalizedEx
                TxApprovedNsf -> Left NsfEx
                TxOtherError  -> Left RedundantVoteEx
                _             -> 
                    let
                        eUpdatedSendRequest = applyEndorsementToSendTx sendRequest (a_balance a) endorsement 
                    in
                    case eUpdatedSendRequest of
                        Left ex -> Left ex
                        Right updatedSendRequest -> 
                            Right (Account {
                                    a_sendTxs = newSendTxs
                                    , a_signers= a_signers a
                                    , a_requiredSigs = a_requiredSigs a
                                    , a_balance= newBalance
                                    , a_accountId= a_accountId a
                                    } 
                                    , fst updatedSendRequest)
                                where
                                    newSendTxs = (\oldSendRequest -> 
                                            if btx_createdDateTime (stx_base oldSendRequest) == btx_createdDateTime (stx_base sendRequest) 
                                                then fst updatedSendRequest
                                                else oldSendRequest
                                            ) <$> a_sendTxs a
                                    -- compute the new SendRequest state
                                    -- if approved
                                    -- TODO EE! issue with newBalance!  Check for NSF
                                    newBalance = 99999

-- For the supplied SendRequest and account balance, apply the endorsement, compute and return the new request state and account balance
applyEndorsementToSendTx :: SendRequestTx -> Int -> EndorsementTx -> Either RequestException (SendRequestTx, Int)
applyEndorsementToSendTx sendRequest oldAccountBalance endorsement = 
    case btx_txState (stx_base sendRequest) of
        TxPendingEndorsement -> if isNowApproved 
            then if stx_sendAmount sendRequest >= oldAccountBalance
                    then Right (updatedSendRequest, oldAccountBalance - stx_sendAmount sendRequest ) -- TODO EE!! verify
                    else Left NsfEx
            else Right (updatedSendRequest, oldAccountBalance)
            where 
                isNowApproved = True -- TODO EE! fix
                updatedSendRequest = SendRequestTx {
                    stx_sendAmount= stx_sendAmount sendRequest
                    , stx_recipient= stx_recipient sendRequest
                    , stx_base=AccountRequestTxBase {
                        btx_txState= btx_txState base -- TODO EE! update
                        , btx_txId= btx_txId base
                        , btx_txCreator= btx_txCreator base
                        , btx_endorsementTxs= btx_endorsementTxs base <> [endorsement]
                        , btx_createdDateTime= btx_createdDateTime base
                        , btx_accountId= btx_accountId base
                        }
                    } 
                    where base = stx_base sendRequest
        _ -> Left AlreadyFinalizedEx        