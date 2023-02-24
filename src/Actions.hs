module Actions where

import Lib
import Types
    -- imports (Wallet, WalletTx, WalletVoteTx, WalletException, Vk, Sk, AppHost, WalletState)
import Data.Char ( toUpper )

getUpperChar :: IO Char
getUpperChar = toUpper <$> getChar

runApp :: IO ()
runApp = do
    putStrLn "Start with sample Wallet? [Y/N]"
    char <- getUpperChar
    if char == 'Y'
        then
            putStrLn "Initializing with sample wallet" >>
            startWallet _TEST_WALLET_
        else
            putStrLn "Initializing with new empty wallet" >>
            startWallet _TEST_WALLET_ -- revise to empty Wallet

promptWalletAction :: IO ()
promptWalletAction = putStrLn "WALLET MODE -- Choose command:\n 1. List Accounts\n 2. Select Account \n 3. Add Account"

startWallet :: Wallet -> IO ()
startWallet w = do
    -- print w
    promptWalletAction
    char <- getUpperChar
    if char == '1' then
        case processWalletAction w char of
            Right updatedWallet -> startWallet updatedWallet
            Left ex -> do print ex
    else
        pure ()

processWalletAction :: Wallet -> Char -> Either WalletException Wallet
processWalletAction w c = case c of 
    '1' -> pure Right listAccounts w
    'q' -> Left WalletException2
    _ -> Left WalletException1

listAccounts :: IO Wallet -> IO (Either WalletException Wallet)
listAccounts w = do
    ww <- w -- change to a for comprehension?
    putStr "wallet account"
    pure (Right ww)

listAccount :: Account -> IO ()
listAccount = print




listAllCmds :: IO ()
listAllCmds = undefined

readCmd :: IO Int
readCmd = undefined

listAllWallets :: IO ()
listAllWallets = undefined

promptForTx :: IO()
promptForTx = undefined


