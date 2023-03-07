module Actions where

import Lib
import Types
    -- imports (Wallet, WalletTx, WalletVoteTx, WalletException, Vk, Sk, AppHost, WalletState)
import Data.Char ( toUpper, digitToInt, intToDigit )
import Control.Monad
import Foreign (Storable(sizeOf))
import System.IO
import GHC.IO.Handle.Internals (flushBuffer, flushByteReadBuffer, flushCharReadBuffer)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

runApp :: IO ()
runApp = do 
    -- disable line buffering to prevent print issues in compiled executable:
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering

    putStrLn "Start with sample Wallet? [Y/N]"
    char <- getUpperChar
    if char == 'Y'
        then do
            putStrLn "Initializing with sample wallet"
            startWallet _TEST_WALLET_
        else do
            putStrLn "Initializing with new empty wallet"
            startWallet $ Wallet [] 999
    pure ()

getUpperChar :: IO Char
-- get first character of an input line
getUpperChar = do
    x <- getLine
    case x of 
        [] -> pure 'z'
        c:_ -> pure (toUpper c)

startWallet :: Wallet -> IO ()
startWallet w = do
    putStrLn "WALLET MODE -- Choose command:\n 1. List Accounts\n 2. Select Account \n 3. Add Account\n 4. Quit"
    char <- getUpperChar
    case char of
        '1' -> do
          listAccounts w
          startWallet  w
        '2' -> do
            print "Enter account number"
            char2 <- getUpperChar
            let idx = digitToInt char2
            -- following is unsafe
            -- todo: delivery reasonable error if out of index
            print $ ah_accounts w !! idx
            -- set the new index, if valid
            let newWallet = Wallet (ah_accounts w) idx
            startAccount newWallet
        '3' -> do
            print "Enter new Account information:"
            print "Number of required signers:"
            print "Number of potential signers (including you):"
            print "Public Key of signer:"
            print "Public Key of signer:"
            -- set random accountId
            let oldAccounts = ah_accounts w
            -- TODO create the real new account with captured params
            let newAccounts = oldAccounts ++ [_TEST_ACCOUNT_]
            startWallet $ Wallet newAccounts (ah_activeAccountIndex w)
        '4' -> do
            pure ()
        _ -> do
            print "Unexpected choice"
            startWallet w

processWalletAction :: Wallet -> Char -> Either WalletException Wallet
processWalletAction w c = case c of 
    '1' -> pure Right listAccounts w
    'q' -> Left WalletException2
    _ -> Left WalletException1

listAccounts :: Wallet -> IO ()
listAccounts w = do
    -- ww <- w -- change to a for comprehension?
    putStrLn "Accounts:"
    when (null $ ah_accounts w ) $
        putStrLn "  (none)"
    unless (null $ ah_accounts w ) $
        -- TODO number them and pretty print
        -- TODO Indicate which one is active, if set to a valid one
        forM_ (ah_accounts w) listAccount

listAccount :: Account -> IO ()
listAccount = print

menuOptions :: [(Int, String)]
menuOptions = [(1, "Print Account")
                , (2, "Print All Txs")
                , (3, "Print Pending Txs")
                , (4, "Set User Vk")
                , (5, "Create Spend Tx")
                , (6, "Vote on Pending Spend Tx")
                , (7, "Modify or Vote on Proposed Signers or Threshold")
                , (8, "Return to Wallet")
                ]

listMenuOptions :: [(Int, String)] -> IO ()
listMenuOptions [] = pure ()
listMenuOptions (a : as) = do
    putChar $ intToDigit $ fst  a
    putStr ". "
    putStrLn $ snd a
    listMenuOptions as

startAccount :: Wallet -> IO ()
startAccount w = do
    print "ACCOUNT MODE -- Choose command:" 
    listMenuOptions menuOptions
    
    char <- getUpperChar
    case char of
        '1' -> do
          listAccounts w
          startAccount  w
        '9' ->
            startWallet w
        _ -> print "unexpected entry" >>
            startAccount w

listAllCmds :: IO ()
listAllCmds = undefined

readCmd :: IO Int
readCmd = undefined

listAllWallets :: IO ()
listAllWallets = undefined

promptForTx :: IO()
promptForTx = undefined


