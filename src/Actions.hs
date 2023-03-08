{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Actions where

import Lib
import Types
    -- imports (Wallet, WalletTx, WalletVoteTx, WalletException, Vk, Sk, AppHost, WalletState)
import Data.Char ( toUpper, digitToInt, intToDigit )
import Control.Monad
import Foreign (Storable(sizeOf))
import System.IO
import GHC.IO.Handle.Internals (flushBuffer, flushByteReadBuffer, flushCharReadBuffer)
import Data.IntMap (IntMap, size)
import qualified Data.IntMap as IntMap
import Text.ParserCombinators.ReadP (count)

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
    putStrLn "WALLET MODE -- Choose command:\n 1. List Accounts\n 2. Select Account \n 3. Add Account\n 4. Authenticate \n 5. Quit"
    char <- getUpperChar
    case char of
        '1' -> do
          listAccounts w
          startWallet  w
        '2' -> do
            putStrLn "Enter account number:"
            char2 <- getUpperChar
            let idx = digitToInt char2
            
            if idx > length (ah_accounts w) - 1 then 
                do
                    putStrLn "Number out of range."
                    startWallet w
                else do
                    print $ ah_accounts w !! idx
                    let newWallet = Wallet (ah_accounts w) idx
                    startAccount newWallet
        '3' -> do
            putStrLn "Enter new Account information:"
            putStrLn "  Number of required signers:"
            -- TODO add error checking
            x <- getUpperChar
            let thresholdNum = digitToInt x
            putStrLn "  Number of potential signers (including you):"
            -- TODO add error checking
            y <- getUpperChar
            let numPotentialSigners = digitToInt y
            putStrLn "  Enter VKeys of other signers:"
            additionalSigners <- getAddlSigner [] numPotentialSigners
            let allSigners = [_TEST_ALICE_VK_, _TEST_BOB_VK_]
            let xxxx = Account 
            let oldAccounts = ah_accounts w
            
            -- TODO create the real new account with captured params
            let newAccounts = oldAccounts ++ [_TEST_ACCOUNT_]
            startWallet $ Wallet newAccounts (ah_activeAccountIndex w)
        -- Authenticate
        '4' -> do
            putStrLn "Enter public key:"
            vk <- getLine
            putStrLn "Enter private key:"
            sk <- getLine
            if elem vk _TEST_Vks_ && elem sk _TEST_Sks_ then do
                putStrLn $ "Successfully authenticated as " <> vk
                -- TODO record the authentication in a new wallet
                startWallet w
            else do
                putStrLn "Invalid match, unable to authenticate"
                startWallet w
        -- Exit
        '5' -> do
            pure ()
        _ -> do
            print "Unexpected choice"
            startWallet w

getAddlSigner :: [Vk] -> Int -> IO [Vk]
getAddlSigner vks 0 = pure vks
getAddlSigner vks numAddlSigners = do
    putStrLn $ "  Public Key of signer " ++ [intToDigit numAddlSigners] ++ ": "
    vk <- getLine
    -- TODO check validity here
    let newVks = vks ++ [vk]
    if numAddlSigners > 0 then
        getAddlSigner newVks (numAddlSigners - 1)
    else
        pure newVks

    

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
    unless (null $ ah_accounts w ) $ do
        let zah_accounts = zip [0,1..] $ ah_accounts w
        -- TODO number them and pretty print
        -- TODO Indicate which one is active, if set to a valid one
        forM_ (zah_accounts) listAccount

listAccount :: (Int, Account) -> IO ()
listAccount (i,a) = do
    putStr [intToDigit i] >> putStr " "
    printAccount a

printAccount :: Account -> IO ()
printAccount a = do
    -- TODO make this prettier, with labels2
    putStrLn $ show a
    putStrLn ""

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
    let activeAccountIndex = ah_activeAccountIndex w
    let activeAccount = ah_accounts w !! activeAccountIndex
    print "ACCOUNT MODE -- Choose command:" 
    listMenuOptions menuOptions
    
    char <- getUpperChar
    case char of
        
        '1' -> do
          -- Print Account
          printAccount activeAccount
          startAccount  w
        
        '2' -> do
            -- Print All Txs
            print "Not yet implemented"
            startAccount w
        '3' -> do
            -- Print Pending Txs
            print "Not yet implemented"
            startAccount w
        '4' -> do
            -- Set User Vk
            print "Not yet implemented"
            startAccount w
        '5' -> do
            -- Create Spend Tx
            print "Not yet implemented"
            startAccount w
        '6' -> do
            -- Vote on Pending Spend Tx
            print "Not yet implemented"
            startAccount w
        '7' -> do
            -- Modify or Vote on Proposed Signers or Threshold
            startAccount w
        '8' ->
            -- Return to Wallet
            startWallet w
        _ -> print "Unexpected entry!" >>
            startAccount w

listAllWallets :: IO ()
listAllWallets = undefined

promptForTx :: IO()
promptForTx = undefined


