module Lib where

import Types
import Data.List (intercalate, elemIndex, transpose)
import Control.Monad
import Control.Monad.State
import Data.Char (toUpper)

mkWallet :: [Account] -> State Wallet ()
mkWallet as = put $ Wallet as Nothing Nothing -- no active account and not authenticated

addOrUpdateSpendTx :: Account -> SpendRequestTx -> Either RequestException Account
addOrUpdateSpendTx = undefined

-- functions to help UI listings
getPendingTxsForVk :: ()
getPendingTxsForVk = undefined

-- authenticate
authenticate :: Vk -> Sk -> Bool
authenticate = undefined

-- Updates to Wallet
addOrUpdateAccount :: Wallet -> Account -> Either RequestException Wallet
addOrUpdateAccount = undefined

-- make sure an account update doesn't violate certain incremental constraints
verifyAccountUpdate :: Account -> Account -> Either RequestException Account
verifyAccountUpdate = undefined

-- vote on various transaction types
endorseSpendTx :: SpendRequestTx -> AccountTxVoteTx -> Either RequestException SpendRequestTx
endorseSpendTx = undefined