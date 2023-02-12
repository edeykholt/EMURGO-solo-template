module Lib where

import Types
-- import Types (Wallet, WalletTx, WalletVoteTx, WalletException, Vk, Sk, AppHost, WalletState)
import Data.List (intercalate, elemIndex, transpose)

listAllWallets :: [] Wallet
listAllWallets = undefined

-- ApproveTx below should perhaps be (Wallet -> SpendTx -> ApproveTx -> Either Ex SpendTx )
applyTx :: Wallet -> Account -> AccountRequest -> AccountRequestTx -> Either RequestException Wallet
applyTx = undefined

getAllTxs :: Wallet -> Account -> [] AccountRequestTx
getAllTxs = undefined

getPendingTxsForVk :: [] AccountRequestTx
getPendingTxsForVk = undefined

getTxsInState :: Wallet -> AccountRequestState ->  [] AccountRequestTx
getTxsInState = undefined

authenticate :: Wallet -> Vk -> Sk -> Bool
authenticate = undefined

createWallet :: ()
createWallet = undefined

createSpendRequest :: Wallet -> Vk -> Int -> Int -> Either RequestException AccountRequestTx
createSpendRequest = undefined

approveOrRejectTx :: AccountRequest -> AccountRequestTx -> AccountTxVote -> Either RequestException AccountRequest
approveOrRejectTx = undefined



