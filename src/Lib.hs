module Lib where

import Types
import Data.List (intercalate, elemIndex, transpose)
-- import Control.Monad
-- import Control.Monad.State
import Data.Char (toUpper)

-- mkWallet :: [Account] -> State Wallet ()
-- mkWallet as = put $ Wallet as Nothing Nothing -- no active account and not authenticated

addOrUpdateSpendTx :: Account -> SpendRequestTx -> Either RequestException Account
addOrUpdateSpendTx = undefined

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