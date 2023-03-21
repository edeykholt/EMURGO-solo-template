module Lib where

import Types
import Data.List (intercalate, elemIndex, transpose, nub)
import Data.Char (toUpper, intToDigit)
import Data.Time (UTCTime)
import Text.XML.HXT.Core (a_name, intToHexString)
import Data.Either (fromRight)
import Control.Monad (forM_)
import Data.Data (typeOf)
import Data.Bool (bool)

-- Add a SendRequest to an account, given a requestor, recipient, amount, account, and requestedDateTime.
addSendRequestTx :: Vk -> Vk -> Int -> Account -> UTCTime -> Either RequestException Account
addSendRequestTx requestor recipient amt (Account id signers bal threshhold sendRequests) utcTime 
    | amt < 0 
        = Left OtherEx
    | notElem requestor _TEST_Vks_ || notElem recipient _TEST_Vks_
        = Left OtherEx
    | otherwise
        = Right $ Account id signers bal threshhold $ sendRequests ++ [SendRequestTx {
                        stx_base            = AccountRequestTxBase {
                            btx_txState             = TxPendingEndorsement
                            , btx_txId              = "" -- TODO unused - remove
                            , btx_txCreator         = requestor
                            , btx_endorsementTxs    = []
                            , btx_createdDateTime   = utcTime
                            , btx_accountId         = id}
                        , stx_recipient     = recipient
                        , stx_sendAmount    = amt
                        }]

-- Check if the Vk and Sk pair is authentic.  A genuine implementation would instead use cryptographic signing and verification
isAuthenticatedPair :: Vk -> Sk -> Bool
-- For now, just see if the vk and sk pair is in the test set
-- TODO confirm these are a matching pair, not just both in the test lists. Will require some refactoring
isAuthenticatedPair vk sk = elem vk _TEST_Vks_ && elem sk _TEST_Sks_ 

-- update the matching account in the wallet
replaceAccount :: Wallet -> Account -> Either WalletException Wallet
replaceAccount w ra =
    let 
        oldAccounts2 = ah_accounts w
        oldAccounts = ah_accounts w
        newAccounts = map (\account -> if a_accountId ra == a_accountId account then ra else account) oldAccounts
    in
    -- TODO implement verification replacement worked. For now, assuming it did
    let newWallet = Wallet newAccounts (ah_activeAccountIndex w) (ah_authenticatedVk w) in
        if areVksUnique $ a_signers ra
            then 
                Right newWallet
            else
                Left AccountSignersNotUnique

-- check for uniqueness of signers
areVksUnique :: [Vk] -> Bool
areVksUnique xs = length (nub xs) == length xs
-- bad implementation:
    -- areVksUnique vks = foldl (\acc vk -> acc && notElem vk (tail vks)) True vks


-- generate a nicely formatted string of the wallet
prettyWallet :: Wallet -> String
prettyWallet w = prettyAccountsWithNum $ ah_accounts w

-- generate a nicely formatted string ...
prettyAccountsWithNum :: [Account] -> String
prettyAccountsWithNum accounts =
            let numberedAccounts = zip [0,1..] accounts in
            unlines $ fmap (\(i, a) -> " #" ++ show i ++ " " ++ a_accountId a ) numberedAccounts

-- generate a nicely formatted string ...
prettyAccount :: Account -> String
prettyAccount a =
    unlines [
        " Account Id: " ++ a_accountId a
        , " Signers: " ++ intercalate ", " (a_signers a)
        , " Required Endorsers: " ++ show (a_requiredSigs a)
        , " Balance: " ++ show (a_balance a)
        , " Send Requests: \n" ++ prettyRequests (a_sendTxs a)
    ]

-- generate a nicely formatted string ...
prettyRequests :: [SendRequestTx] -> String
prettyRequests [] = "   (no Send Requests)"
prettyRequests requests =
    intercalate " " $ map prettyRequestWithNum $ zip [0, 1..] requests

-- generate a nicely formatted string ...
prettyRequestWithNum :: (Int, SendRequestTx) -> String
prettyRequestWithNum (i, request) = unlines [
        "  #" ++ [intToDigit i]
        , prettyRequest request
        ]

-- generate a nicely formatted string ...
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

-- generate a nicely formatted string ...
prettyEndorsers :: [EndorsementTx] -> String
prettyEndorsers [] = "(none)"
prettyEndorsers endorsers = intercalate ", " $ map prettyEndorser endorsers

-- generate a nicely formatted string ...
prettyEndorser :: EndorsementTx -> String
prettyEndorser endorser = show (atxv_approverVk endorser)

-- Find the index of a SendRequestTx in a list, based on its dateTime. This is an admitedly weak identifier that should be improved in the future.
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

-- For the supplied account, sendRequest, and endorsement, update the account and sendRequest, and return them
applyEndorsement :: Account -> SendRequestTx -> EndorsementTx -> Either RequestException (Account, SendRequestTx)
applyEndorsement a sendRequest endorsement = 
    let mFoundIndex = findSendRequestIndex (a_sendTxs a) sendRequest in
    case mFoundIndex of
        Nothing -> Left EndorsementTargetNotFoundEx
        Just index -> 
            let oldSendRequest = a_sendTxs a !! index in
            -- verify old sendRequest is still in a pending state that can accept endorsements
            case btx_txState $ stx_base oldSendRequest of
                TxApproved              -> Left AlreadyFinalizedEx
                TxApprovedNsf           -> Left NsfEx
                TxPendingEndorsement    -> 
                    let
                        -- TODO verify the endorsement points to the sendRequest, which points to the Account
                        -- isReferenceIntegrity ...
                        -- if the count of endorsements, including this one plus the creator's, is equal or greater than the required sigs, it will be approved if funds are available
                        isApprovedPendingFunds = 2 + length (btx_endorsementTxs $ stx_base sendRequest) >= a_requiredSigs a
                        -- check if the endorser is either the same as the sendRequest's creator or is a prior endorser on this sendRequest
                        isRepeatEndorser = atxv_approverVk endorsement == btx_txCreator (stx_base sendRequest) ||
                            foldl (\acc e -> acc || atxv_approverVk e == atxv_approverVk endorsement ) False (btx_endorsementTxs $ stx_base sendRequest)
                    in
                    if isRepeatEndorser
                        then
                            Left RedundantVoteEx
                        else if isApprovedPendingFunds
                            then
                                -- check if adequate funds are available, and update newBalance and state
                                if a_balance a >= stx_sendAmount sendRequest
                                    then
                                        let newBalance = a_balance a - stx_sendAmount sendRequest in
                                        applyEndorsement2 a sendRequest endorsement newBalance TxApproved
                                    else
                                        applyEndorsement2 a sendRequest endorsement (a_balance a) TxApprovedNsf
                            else
                                applyEndorsement2 a sendRequest endorsement (a_balance a) TxPendingEndorsement


-- This helper is used by the above applyEndorsement with similar signature but also providing newBalance and newState
applyEndorsement2 :: Account -> SendRequestTx -> EndorsementTx -> Int -> TxState -> Either RequestException (Account, SendRequestTx)
applyEndorsement2 a newSendRequestTx endorsement newBalance newState = 
    let
        -- create the list of newSendTxs via an fmap across the account's send transactions and update it with one matching based on creation timestamp
        newSendTxs = (\oldSendRequest -> 
                if btx_createdDateTime (stx_base oldSendRequest) == btx_createdDateTime (stx_base newSendRequestTx) 
                then 
                    -- TODO confirm integrity of inputs, that the supplied endorsement references supplied sendRequest
                    let base = stx_base oldSendRequest 
                    in
                        SendRequestTx {
                            stx_sendAmount = stx_sendAmount oldSendRequest, 
                            stx_recipient = stx_recipient oldSendRequest, 
                            stx_base = AccountRequestTxBase {
                                btx_txState = newState -- This is being updated
                                , btx_txId = btx_txId base
                                , btx_txCreator = btx_txCreator base
                                , btx_endorsementTxs = btx_endorsementTxs base <> [endorsement] -- This is being appended
                                , btx_createdDateTime = btx_createdDateTime base
                                , btx_accountId = btx_accountId base
                            }
                        }
                else 
                    oldSendRequest
            ) <$> a_sendTxs a
        -- get the updatedSendRequest out of the above newSentTxs
        mUpdatedSendRequest = findSendRequest newSendTxs newSendRequestTx
    in
        case mUpdatedSendRequest of
            Nothing                 -> Left OtherEx
            Just updatedSendRequest -> Right ( 
                Account {
                    a_signers = a_signers a
                    , a_sendTxs = newSendTxs -- updated
                    , a_requiredSigs = a_requiredSigs a
                    , a_balance = newBalance -- updated
                    , a_accountId = a_accountId a
                    }
                , updatedSendRequest)

-- check if the transaction state is one of the final states
isFinalTxState :: TxState -> Bool
isFinalTxState s = s /= TxPendingEndorsement

-- maybe find the matching sendRequest in list based createdDateTime of a provided sendRequest
findSendRequest :: [SendRequestTx] -> SendRequestTx -> Maybe SendRequestTx
findSendRequest [] _ = Nothing
findSendRequest (sr:srs) psr = if btx_createdDateTime (stx_base psr) == btx_createdDateTime (stx_base sr)
    then Just sr
    else findSendRequest srs psr

-- UI-friendly show messages for RequestException
instance Show RequestException where 
    show re = case re of 
        NsfEx -> "Not Sufficient Funds."
        UnauthorizedSignerEx -> "Unauthorized Signer."
        RedundantVoteEx -> "You already endorsed this Spend Request."
        AlreadyFinalizedEx -> "The spend request is already finalized."
        _ -> "Other exception."
    