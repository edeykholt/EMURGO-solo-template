{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use print" #-}
{-# LANGUAGE FlexibleContexts #-}
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
import GHC.IO hiding (liftIO)
import GHC.Num (integerToNatural)
import GHC.Natural (mkNatural)
import Control.Monad.Cont hiding (liftIO)
import Control.Monad.State ( MonadState (get, put), evalStateT, liftIO)


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
            -- startWallet _TEST_WALLET_
            evalStateT startWallet _TEST_WALLET_
        else do
            putStrLn "Initializing with new empty wallet"
            -- startWallet $ Wallet [] Nothing Nothing
            evalStateT startWallet $ Wallet [] Nothing Nothing
    pure ()

getUpperChar :: IO Char
-- get first character of an input line
getUpperChar = do
    x <- getLine
    case x of 
        [] -> pure 'z'
        c:_ -> pure (toUpper c)

startWallet :: (MonadIO m, MonadState Wallet m) => m ()
startWallet = do
    Wallet accounts maybeActiveAccountIndex maybeAuthenticatedVk <- get
    liftIO $ putStr "WALLET MODE _____" 
    -- let maybeAuthenticatedVk = ah_authenticatedVk w
    if isNothing maybeAuthenticatedVk
        then do
            liftIO $ putStrLn "No user currently authenticated _________"
            -- authenticate and restart
            newWallet <- liftIO $ authenticatePk $ Wallet accounts maybeActiveAccountIndex maybeAuthenticatedVk
            put newWallet
            startWallet
        else do
            liftIO $ putStrLn $ "Authenticated as " ++ fromJust maybeAuthenticatedVk ++ "______"
            -- continue below

            liftIO $ listAccounts accounts
            liftIO $ putStrLn "Choose a wallet command:\n 1. List accounts with detail\n 2. Select Account \n 3. Add Account\n 4. Authenticate \n 9. Exit App"
            selectedCmd <- liftIO getUpperChar
            case selectedCmd of
                '1' -> do
                    -- List Accounts
                    -- TODO update to detailed list
                    liftIO $ listAccounts accounts
                    startWallet
                '2' -> do
                    -- Select Account
                    if null accounts
                        then do
                            liftIO $ putStrLn "No accounts configured yet"
                            startWallet
                        else do
                            liftIO $ putStrLn "Enter account number:"
                            char2 <- liftIO getUpperChar
                            let selectedIdx = digitToInt char2
                    
                            -- check if index is valid -- or the active index is not yet set
                            if selectedIdx + 1 > length accounts -- ) || isNothing (ah_activeAccountIndex w)
                                then do
                                    liftIO $ putStrLn "Number out of range."
                                    startWallet
                                else do
                                    liftIO $ print $ accounts !! selectedIdx
                                    let newWallet = Wallet accounts (Just selectedIdx) maybeAuthenticatedVk
                                    put newWallet
                                    liftIO $ startAccount newWallet
                                    startWallet
                '3' -> do
                    -- Add a new account

                    -- precondition is that user if authenticated
                    if isNothing maybeAuthenticatedVk 
                        then do
                            liftIO $ putStrLn "User must be authenticated to add an account"
                            startWallet
                        else do
                            liftIO $ putStrLn "Enter new Account information..."
                            
                            liftIO $ putStrLn "  Account Name:"
                            accountName <- liftIO getLine
                            
                            liftIO $ putStrLn "  Number of required signers:"
                            x <- liftIO getUpperChar
                            let thresholdNum = digitToInt x
                            let isInvalidThreshold = thresholdNum < 2 || thresholdNum > 9
                            if isInvalidThreshold
                                then do
                                    liftIO $ putStrLn "Invalid number of required signers"
                                    startWallet
                                else do
                                    liftIO $ putStrLn "  Number of potential signers (including you):"
                                    y <- liftIO getUpperChar
                                    let numPotentialSigners = digitToInt y
                                    let isInvalidPotential = numPotentialSigners < thresholdNum || numPotentialSigners > 9
                                    if isInvalidPotential
                                        then do
                                            liftIO $ putStrLn "Invalid number of potential signers"
                                            startWallet
                                        else do
                                            -- get VKeys of additional signers, in addition to the authenticated one
                                            liftIO $ putStrLn "  Enter VKeys of other signers:"
                                            let existingSigners = [fromJust maybeAuthenticatedVk]
                                            allSigners <- liftIO $ addSigner existingSigners (numPotentialSigners - 1)

                                            -- Now that we have we've collected parameters, create the proposed new Account
                                            let newAccount = Account accountName allSigners thresholdNum 100 []

                                            -- In the current design, no approval is needed to create the account. There's a known risk to usage is if it is created with bad Vks.
                                            currentTime <- liftIO getCurrentTime
                                            -- let newAccountBaseTx = AccountRequestTxBase (a_accountId newAccount) "txId" (fromJust maybeAuthenticatedVk) currentTime TxApproved []
                                            
                                            -- Update the wallet with the new Account
                                            let newAccounts = accounts ++ [newAccount]
                                            let newIndex = length newAccounts - 1
                                            let newWallet = Wallet newAccounts (Just newIndex) maybeAuthenticatedVk
                                            put newWallet

                                            -- print newWallet
                                            liftIO $ putStrLn "Added new account to wallet"

                                            startWallet
                
                '4' -> do
                    -- Authenticate and restart wallet
                    w <- get
                    newWallet <- liftIO $ authenticatePk w
                    put newWallet
                    startWallet
                
                '9' -> do
                    -- Exit
                    pure ()

                _ -> do
                    liftIO $ print "Unexpected choice"
                    startWallet

authenticatePk :: Wallet -> IO Wallet
authenticatePk w = do
    putStrLn "To authenticate, enter your public key:"
    vk <- getLine
    putStrLn "Now, enter your private key:"
    sk <- getLine
    if isAuthenticatedPair vk sk
        then do
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
    if vk `notElem` _TEST_Vks_
        then do
            putStrLn "Public key is not in known list. Try again."
            addSigner vks numAddlSigners
        else do
            let newVks = vks ++ [vk]
            if numAddlSigners > 0 then
                addSigner newVks (numAddlSigners - 1)
            else
                pure newVks

listAccounts :: [Account] -> IO ()
listAccounts accounts = do
    putStrLn "Accounts:"
    if null accounts
        then do
            putStrLn "  (none)"
        else do
            let zah_accounts = zip [0,1..] accounts
            -- TODO number them and pretty print
            -- TODO Indicate which one is active, if set to a valid one
            forM_ zah_accounts listAccount

listAccount :: (Int, Account) -> IO ()
listAccount (i,a) = do
    putStr "#" >> putStr [intToDigit i] >> putStr " "
    putStrLn $ a_accountId a

printAccount :: Account -> IO ()
printAccount a = do
    -- TODO make this prettier, with labels2
    putStrLn $ show a

menuOptions :: [(Int, String)]
menuOptions = [(1, "Print Account")
                , (2, "Print All Txs")
                , (3, "Print Pending Txs")
                , (4, "Create Spend Tx")
                , (5, "Endorse Pending Spend Tx")
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
    let maybeAccountIndex = ah_activeAccountIndex w
    case maybeAccountIndex of
        Nothing -> do
            putStrLn "Expected active account index to be set"
            pure ()
        Just accountIndex -> do
            let activeAccount = ah_accounts w !! accountIndex
    
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
                    -- Create Spend Tx
                    print "Not yet implemented"
                    startAccount w
                '5' -> do
                    -- Endorse Pending Spend Tx
                    print "Not yet implemented"
                    startAccount w
                '9' ->
                    -- Exit account. Return to wallet
                    pure ()
                _ -> print "Unexpected entry!" >>
                    startAccount w
            pure () -- return to caller, e.g. wallet mode