module Lib where

import Types
-- import Types (Wallet, WalletTx, WalletVoteTx, WalletException, Vk, Sk, AppHost, WalletState)
import Data.List (intercalate, elemIndex, transpose)

listAllWallets :: [] Wallet
listAllWallets = undefined

-- ApproveTx below should perhaps be (Wallet -> SpendTx -> ApproveTx -> Either Ex SpendTx )
applyTx :: Wallet -> WalletTx -> WalletVoteTx -> Either WalletException WalletTx
applyTx = undefined

getAllTxs :: [] WalletTx
getAllTxs = undefined

getPendingTxs :: [] WalletTx
getPendingTxs = undefined

getPendingTxsForVk :: [] WalletTx
getPendingTxsForVk = undefined

filterTxsForSk :: [] WalletTx -> Vk -> [] WalletTx
filterTxsForSk = undefined

getTxsInState :: AppHost -> WalletState -> [] WalletTx
getTxsInState = undefined

authenticate :: AppHost -> Vk -> Sk -> Bool
authenticate = undefined

createWallet :: ()
createWallet = undefined

createSpendRequest :: Wallet -> Vk -> Int -> Int -> Either WalletException WalletTx
createSpendRequest = undefined

approveOrRejectTx :: WalletTx -> Bool -> Either WalletException WalletTx
approveOrRejectTx = undefined



