module Lib where

import Types
import Data.List (intercalate, elemIndex, transpose)

-- add or update transaction in account ----------------------------------------------------------------------
addOrUpdateCreateTx :: Account -> CreateTx -> Either RequestException Account
addOrUpdateCreateTx = undefined

addOrUpdateSpendTx :: Account -> SpendTx -> Either RequestException Account
addOrUpdateSpendTx = undefined

addOrUpdateAddSignerTx :: Account -> AddSignerTx -> Either RequestException Account
addOrUpdateAddSignerTx = undefined

addOrUpdateRemoveSignerTx :: Account -> RemoveSignerTx -> Either RequestException Account
addOrUpdateRemoveSignerTx = undefined

addOrUpdateUpdateNumSignersTx :: Account -> UpdateNumSignersTx -> Either RequestException Account
addOrUpdateUpdateNumSignersTx = undefined

-- functions to help UI listings
getAllWalletTxs ::  ()
getAllWalletTxs = undefined

getPendingTxsForVk :: ()
getPendingTxsForVk = undefined

getTxsInState :: ()
getTxsInState = undefined

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
voteOnSpendTx :: SpendTx -> AccountTxVoteTx -> Either RequestException SpendTx
voteOnSpendTx = undefined
-- voteOnAddSignerTx
-- voteOnRemoveSignerTx
-- voteOnUpdateNumSignersTx