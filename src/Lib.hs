module Lib where

import Types
-- import Types (Wallet, WalletTx, WalletVoteTx, WalletException, Vk, Sk, AppHost, WalletState)
import Data.List (intercalate, elemIndex, transpose)

listAllAccounts :: [] Account
listAllAccounts = undefined

applyTx :: Account -> AccountRequest -> AccountRequestTx -> Either RequestException Account
applyTx = undefined

getAllWalletTxs ::  Wallet -> Account -> [] AccountRequestTx
getAllWalletTxs = undefined

getPendingTxsForVk :: [] AccountRequestTx
getPendingTxsForVk = undefined

getTxsInState :: Wallet -> AccountRequestState ->  [] AccountRequestTx
getTxsInState = undefined

authenticate :: Account -> Vk -> Sk -> Bool
authenticate = undefined

createWallet :: ()
createWallet = undefined

addAccount :: Wallet -> Account
addAccount :: undefined

createSpendRequest :: Wallet -> Vk -> Int -> Int -> Either RequestException AccountRequestTx
createSpendRequest = undefined

approveOrRejectTx :: AccountRequest -> AccountRequestTx -> AccountTxVote -> Either RequestException AccountRequest
approveOrRejectTx = undefined



