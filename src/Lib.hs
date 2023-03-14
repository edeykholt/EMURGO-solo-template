module Lib where

import Types
import Data.List (intercalate, elemIndex, transpose)
-- import Control.Monad
-- import Control.Monad.State
import Data.Char (toUpper, intToDigit)
import Data.Time (UTCTime)
import Text.XML.HXT.Core (a_name, intToHexString)
import Data.Either (fromRight)


addSpendRequestTx :: Vk -> Vk -> Int -> Account -> UTCTime -> Either RequestException Account
addSpendRequestTx requestor recipient amt (Account id signers bal threshhold spendRequests) utcTime = 
    -- TODO verify requestor and recipient
    -- TODO verify amt > 0
    Right $ Account id signers bal threshhold $ spendRequests ++ [SpendRequestTx {
        stx_base=AccountRequestTxBase {
            btx_txState=TxRequested
            , btx_txId="123412341234"
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

-- Updates to Wallet
addOrUpdateAccount :: Wallet -> Account -> Either RequestException Wallet
addOrUpdateAccount = undefined

-- make sure an account update doesn't violate certain incremental constraints
verifyAccountUpdate :: Account -> Account -> Either RequestException Account
verifyAccountUpdate = undefined

-- vote on various transaction types
endorseSpendTx :: SpendRequestTx -> AccountTxVoteTx -> Either RequestException SpendRequestTx
endorseSpendTx = undefined

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

-- prettyAccounts :: [Account] -> String

prettyAccount :: Account -> String
prettyAccount a =
    unlines [
        "Account Id: " ++ a_accountId a
        , "Signers: " ++ intercalate ", " (a_signers a)
        , "Required Endorsers: " ++ show (  a_requiredSigs a )
        , "Balance: " ++ show (a_balance a )
        , "Spend Requests... \n" ++ prettyRequests (a_spendTxs a)
    ]

prettyRequests :: [SpendRequestTx] -> String
prettyRequests [] = "(no Spend Requests)"
prettyRequests requests =
    intercalate "\n" $ map prettyRequest $ zip [1..] requests
    where
        prettyRequest :: (Int, SpendRequestTx) -> String
        prettyRequest (i, request) = unlines [
            "Request #" ++ [intToDigit i]
            , "Recipient: " ++ show (stx_recipient request)
            , "Amount: " ++ show (stx_spendAmount request)
            , "Requester: " ++ btx_txCreator base
            , "Requested at: " ++ show (btx_createdDateTime base)
            , "Workflow State: " ++ show (btx_txState base)
            , "Additional Endorsers: " ++ prettyEndorsers (btx_endorseTxs base)
            ]
            where base = stx_base request

prettyEndorsers :: [AccountTxVoteTx] -> String
prettyEndorsers [] = "(none)"
prettyEndorsers es = intercalate ", " $ map prettyEndorser es

prettyEndorser :: AccountTxVoteTx -> String
prettyEndorser pe = show (atxv_approverVk pe)

applyEndorsement :: Account -> SpendRequestTx -> AccountTxVoteTx -> Either RequestException Account
applyEndorsement a s e = 
    Right Account {
        a_spendTxs = newSpendTxs
        , a_signers= a_signers a
        , a_requiredSigs = a_requiredSigs a
        , a_balance= newBalance
        , a_accountId= a_accountId a
        }
        where
            -- TODO find existing SpendRequestTx within the account and repalce it
            -- TODO TOTAL hack for now is to append it
            oldSelectedSpendTx = a_spendTxs a !! 0 -- TODO find specific SpendRequestTx in list, and do surgury on it
            ret = applyEndorsementToSpendTx oldSelectedSpendTx (a_balance a) e 
            -- TODO issue with newBalance!
            newBalance = 0
            newSpendTxs = case ret of
                Right (x, y) -> 
                    -- newBalance = y
                    a_spendTxs a ++ [x]
                Left _ -> 
                    -- newBalance = 1
                    a_spendTxs a

-- For the supplied SpendRequest and account balance, apply the endorsement, compute new request state and account balance
applyEndorsementToSpendTx :: SpendRequestTx -> Int -> AccountTxVoteTx -> Either RequestException (SpendRequestTx, Int)
applyEndorsementToSpendTx spendRequest oldAccountBalance endorsement = 
    -- TODO implement real stuff
    -- Either RequestException (SpendRequestTx, Int)
    Right (spendRequest, oldAccountBalance)