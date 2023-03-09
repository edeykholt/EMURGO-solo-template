module Lib where

import Types
import Data.List (intercalate, elemIndex, transpose)

import Control.Monad
import Control.Monad.State
import Data.Char (toUpper)

mkWallet :: [Account] -> State Wallet ()
mkWallet as = put $ Wallet as 999 Nothing -- no active account and not authenticated

-- TODO consider defining a custom typeclass !!!

-- add or update transaction in account ----------------------------------------------------------------------
addOrUpdateCreateTx :: Account -> CreateAccountTx -> Either RequestException Account
addOrUpdateCreateTx = undefined
-- see if Tx already exists in account
-- verify validity of CreateTx, including: 
    -- is the CreateTx itself correct, e.g. signed and within date?
    -- valid for existing state and potential newstate?
    -- if this is the final approval, is there enough funds?

-- TODO
-- Class-based ad-hoc polymorphism
-- We could also have achieved a polymorphic area function over shapes in this way:
-- data Circle = Circle Float
-- data Rect = Rect Float Float
-- class Shape a where
  -- area :: a -> Float

-- instance Shape Circle where
  -- area (Circle r) = pi * r^2
-- instance Shape Rect where
  -- area (Rect length' width') = length' * width'

-- class ApprovableTx atx where
--     approveTx :: atx -> Account -> Maybe Account

addOrUpdateSpendTx :: Account -> SpendRequestTx -> Either RequestException Account
addOrUpdateSpendTx = undefined

addOrUpdateAddSignerTx :: Account -> AddSignerRequestTx -> Either RequestException Account
addOrUpdateAddSignerTx = undefined

addOrUpdateRemoveSignerTx :: Account -> RemoveSignerRequestTx -> Either RequestException Account
addOrUpdateRemoveSignerTx = undefined

addOrUpdateUpdateNumSignersTx :: Account -> UpdateNumSignersRequestTx -> Either RequestException Account
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
voteOnSpendTx :: SpendRequestTx -> AccountTxVoteTx -> Either RequestException SpendRequestTx
voteOnSpendTx = undefined
-- voteOnAddSignerTx
-- voteOnRemoveSignerTx
-- voteOnUpdateNumSignersTx