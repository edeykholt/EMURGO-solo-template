module Lib where
import Types (Tx)

listAllWallets :: [] Wallet
listAllWallets = undefined

-- ApproveTx below should perhaps be (Wallet -> SpendTx -> ApproveTx -> Either Ex SpendTx )
applyTx :: Wallet -> WorkflowTx -> ApproveTx -> Either Ex ApproveTx
applyTx = undefined

getAllTxs :: [] Tx
getAllTxs = undefined

getPendingTxs :: [] Tx
getPendingTxs = undefined

getPendingTxsForVk :: [] Tx
getPendingTxsForVk = undefined

filterTxsForSk :: [] Tx -> Vk -> [] Tx
filterTxsForSk = undefined

getTxsInState :: AppHost -> WalletState -> [] Tx
getTxsInState = undefined

authenticate :: AppHost -> Sj -> Pk -> Bool
authenticate = undefined

createWallet :: ()
createWallet = undefined

createSpendRequest :: Wallet -> Vk -> Int -> Int -> Either Ex WalletTx
createSpendRequest = undefined

approveOrRejectTx :: SpendRequest -> Bool -> Either Ex SpendRequest
approveOrRejectTx = undefined



