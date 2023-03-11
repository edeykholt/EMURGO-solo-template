{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Actions where

import Lib
import Types
import Data.Char ( toUpper, digitToInt, intToDigit )
import Data.Maybe (isJust, fromJust, isNothing, listToMaybe)
import Control.Monad
import Foreign (Storable(sizeOf))
import System.IO
import GHC.IO.Handle.Internals (flushBuffer, flushByteReadBuffer, flushCharReadBuffer)
import Data.IntMap (IntMap, size)
import qualified Data.IntMap as IntMap
import Text.ParserCombinators.ReadP (count)
import Data.Time (getCurrentTime)
import GHC.IO (liftIO)
import GHC.Num (integerToNatural)
import GHC.Natural (mkNatural)

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
            startWallet $ Wallet [] Nothing Nothing
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
    putStr "WALLET MODE _____" 
    let maybeAuthenticatedVk = ah_authenticatedVk w
    if isJust maybeAuthenticatedVk then
        do
            putStrLn $ "Authenticated as " ++ fromJust maybeAuthenticatedVk ++ "______"
        else do
            putStrLn "No user currently authenticated _________"
            -- authenticate and restart
            newWallet <- authenticatePk w
            startWallet newWallet

    putStrLn "Choose a wallet command:\n 1. List Accounts\n 2. Select Account \n 3. Add Account\n 4. Authenticate \n 9. Quit"
    char <- getUpperChar
    case char of
        '1' -> do
            -- List Accounts
            listAccounts w
            startWallet  w
        '2' -> do
            -- Select Account
            putStrLn "Enter account number:"
            char2 <- getUpperChar
            let selectedIdx = digitToInt char2
            
            -- check if index is valid -- or the active index is not yet set
            if selectedIdx + 1 > length (ah_accounts w) -- ) || isNothing (ah_activeAccountIndex w)
                then do
                    putStrLn "Number out of range."
                    startWallet w
                else do
                    print $ ah_accounts w !! selectedIdx
                    let newWallet = Wallet (ah_accounts w) (Just selectedIdx) (ah_authenticatedVk w)
                    startAccount newWallet
        '3' -> do
            -- Add a new account

            -- precondition is that user if authenticated
            let authenticatedUser = ah_authenticatedVk w
            if isNothing authenticatedUser then do
                putStrLn "User must be authenticated to add an account"
                startWallet w
            else do
                putStrLn "Enter new Account information..."
                
                putStrLn "  Account Name:"
                accountName <- getLine
                
                putStrLn "  Number of required signers:"
                x <- getUpperChar
                let thresholdNum = digitToInt x
                -- TODO add error checking, in range of 1-9
                
                putStrLn "  Number of potential signers (including you):"
                y <- getUpperChar
                let numPotentialSigners = digitToInt y
                -- TODO add error checking, in range of threshold-9

                -- get VKeys of additional signers, in addition to the authenticated one
                putStrLn "  Enter VKeys of other signers:"
                let existingSigners = [fromJust $ ah_authenticatedVk w]
                allSigners <- addSigner existingSigners (numPotentialSigners - 1)

                -- Now that we have we've collected parameters, create the proposed new Account
                let newAccount = Account "Shared Account" allSigners thresholdNum 100 Nothing []

                -- For now, there will be no approval needed to create the account. TODO create an approval flow for new account
                currentTime <- getCurrentTime
                let newAccountBaseTx = AccountRequestTxBase (a_accountId newAccount) "txId" (fromJust $ ah_authenticatedVk w) currentTime TxApproved []
                let createAccountTx = CreateAccountTx newAccountBaseTx newAccount
                -- TODO createAccountTx is unused after this, because current design assumes creation doesn't require approval
                
                -- Update the wallet with the new Account
                let newAccounts = ah_accounts w ++ [newAccount]
                let newIndex = length newAccounts - 1
                let newWallet = Wallet newAccounts (Just newIndex) authenticatedUser
                -- print newWallet
                putStrLn "Added new account to wallet"
                startWallet newWallet
        
        '4' -> do
            -- Authenticate and restart wallet
            newWallet <- authenticatePk w
            startWallet newWallet
        
        '9' -> do
            -- Exit
            pure ()

        _ -> do
            print "Unexpected choice"
            startWallet w

authenticatePk :: Wallet -> IO Wallet
authenticatePk w = do
    putStrLn "To authenticate, enter your public key:"
    vk <- getLine
    putStrLn "Now, enter your private key:"
    sk <- getLine
    if elem vk _TEST_Vks_ && elem sk _TEST_Sks_ then do
        putStrLn $ "Successfully authenticated as " ++ vk
        let newWallet = Wallet (ah_accounts w) (ah_activeAccountIndex w) (pure vk)
        pure newWallet
    else do
        putStrLn "Invalid match, unable to authenticate"
        pure w

addSigner :: [Vk] -> Int -> IO [Vk]
addSigner vks 0 = pure vks
addSigner vks numAddlSigners = do
    putStrLn $ "  Public Key of signer " ++ [intToDigit numAddlSigners] ++ ": "
    vk <- getLine
    -- TODO check validity here. Should be in list of sample VKs for this purpose
    let newVks = vks ++ [vk]
    if numAddlSigners > 0 then
        addSigner newVks (numAddlSigners - 1)
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
        forM_ zah_accounts listAccount

listAccount :: (Int, Account) -> IO ()
listAccount (i,a) = do
    putStr "#" >> putStr [intToDigit i] >> putStr " "
    putStrLn $ a_accountId a
    -- printAccount a

printAccount :: Account -> IO ()
printAccount a = do
    -- TODO make this prettier, with labels2
    putStrLn $ show a
    putStrLn ""

menuOptions :: [(Int, String)]
menuOptions = [(1, "Print Account")
                , (2, "Print All Txs")
                , (3, "Print Pending Txs")
                , (4, "Authenticate as different user")
                , (5, "Create Spend Tx")
                , (6, "Endorse Pending Spend Tx")
                -- , (7, "Modify or Vote on Proposed Signers or Threshold")
                , (9, "Return to Wallet")
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
    if isNothing activeAccountIndex
        then do
            putStrLn "Expected active account index to be set"
            pure ()
        else do
            let activeAccount = ah_accounts w !! fromJust activeAccountIndex
    
            putStrLn $ "ACCOUNT MODE ____ account " ++ a_accountId activeAccount ++ ". ___ Authenticated as " ++ fromJust (ah_authenticatedVk w) ++ "______"
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
                    -- Authenticate as different user
                    newWallet <- authenticatePk w
                    startAccount newWallet
                '5' -> do
                    -- Create Spend Tx
                    print "Not yet implemented"
                    startAccount w
                '6' -> do
                    -- Endorse Pending Spend Tx
                    print "Not yet implemented"
                    startAccount w
                '9' ->
                    -- Return to Wallet
                    startWallet w
                _ -> print "Unexpected entry!" >>
                    startAccount w

listAllWallets :: IO ()
listAllWallets = undefined

promptForTx :: IO()
promptForTx = undefined


