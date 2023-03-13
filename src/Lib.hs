module Lib where

import Types
import Data.List (intercalate, elemIndex, transpose)
-- import Control.Monad
-- import Control.Monad.State
import Data.Char (toUpper)
import Data.Time (UTCTime)

-- mkWallet :: [Account] -> State Wallet ()
-- mkWallet as = put $ Wallet as Nothing Nothing -- no active account and not authenticated

-- addOrUpdateSpendTx :: Account -> SpendRequestTx -> Either RequestException Account
-- TODO not yet implemented
--- addOrUpdateSpendTx a _ = testSpendRequestTx _TEST_ALICE_VK_ 10 a

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

-- functions to help UI listings
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