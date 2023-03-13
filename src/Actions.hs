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
import Control.Monad.State ( MonadState (get, put), evalStateT, liftIO, execStateT, StateT (StateT))
import System.Console.ANSI
import Text.XML.HXT.DOM.Util (decimalStringToInt)
import Control.Monad.ST
import Data.Either (fromRight)

runApp :: IO ()
runApp = do 
    -- disable line buffering to prevent print issues in compiled executable:
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering
    promptUser putStr "Start with sample Wallet? [Y/N] "
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

promptUser :: (String -> IO () ) -> String -> IO ()
promptUser f s = do
    setSGR [SetColor Foreground Vivid Blue]
    f s
    setSGR [Reset]

warnUser :: (String -> IO () ) -> String -> IO ()
warnUser f s = do
    setSGR [SetColor Foreground Vivid Red]
    f s
    setSGR [Reset]

emphasisUser :: (String -> IO () ) -> String -> IO ()
emphasisUser f s = do
    setSGR [SetConsoleIntensity BoldIntensity]
    f s
    setSGR [Reset]

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
    liftIO $ emphasisUser putStr "WALLET MODE   Authenticated User: " 
    -- let maybeAuthenticatedVk = ah_authenticatedVk w
    if isNothing maybeAuthenticatedVk
        then do
            liftIO $ emphasisUser putStrLn "none"
            -- authenticate and restart
            newWallet <- liftIO $ authenticatePk $ Wallet accounts maybeActiveAccountIndex maybeAuthenticatedVk
            put newWallet
            startWallet
        else do
            liftIO $ emphasisUser putStrLn $ fromJust maybeAuthenticatedVk
            -- continue below

            liftIO $ listAccounts accounts
            liftIO $ putStrLn "1. List accounts with detail\n2. Enter Account Mode \n3. Add Account\n4. Authenticate \n9. Exit App"
            liftIO $ promptUser putStr "Enter choice: "
            selectedCmd <- liftIO getUpperChar
            case selectedCmd of
                '1' -> do
                    -- List Accounts
                    -- TODO update to detailed list
                    liftIO $ listAccounts accounts
                    startWallet
                '2' -> do
                    -- Enter Account Mode
                    if null accounts
                        then do
                            liftIO $ warnUser putStrLn "No accounts configured yet"
                            startWallet
                        else do
                            liftIO $ promptUser putStr "Enter account number: "
                            char2 <- liftIO getUpperChar
                            let selectedIdx = digitToInt char2
                    
                            -- check if index is valid -- or the active index is not yet set
                            if selectedIdx + 1 > length accounts -- ) || isNothing (ah_activeAccountIndex w)
                                then do
                                    liftIO $ warnUser putStrLn "Number out of range."
                                    startWallet
                                else do
                                    liftIO $ print $ accounts !! selectedIdx
                                    let newWallet = Wallet accounts (Just selectedIdx) maybeAuthenticatedVk
                                    put newWallet
                                    newerWallet <- execStateT startAccount newWallet
                                    put newerWallet
                                    --test
                                    liftIO $ print newerWallet
                                    startWallet
                '3' -> do
                    -- Add a new account

                    -- precondition is that user if authenticated
                    if isNothing maybeAuthenticatedVk 
                        then do
                            liftIO $ warnUser putStrLn "User must be authenticated to add an account"
                            startWallet
                        else do
                            liftIO $ promptUser putStrLn "Enter new Account information..."
                            
                            liftIO $ promptUser putStr "  Account Name: "
                            accountName <- liftIO getLine
                            
                            liftIO $ promptUser putStr "  Number of required signers: "
                            x <- liftIO getUpperChar
                            let thresholdNum = digitToInt x
                            let isInvalidThreshold = thresholdNum < 2 || thresholdNum > 9
                            if isInvalidThreshold
                                then do
                                    liftIO $ warnUser putStrLn "Invalid number of required signers"
                                    startWallet
                                else do
                                    liftIO $ promptUser putStr "  Number of potential signers (including you): "
                                    y <- liftIO getUpperChar
                                    let numPotentialSigners = digitToInt y
                                    let isInvalidPotential = numPotentialSigners < thresholdNum || numPotentialSigners > 9
                                    if isInvalidPotential
                                        then do
                                            liftIO $ warnUser putStrLn "Invalid number of potential signers"
                                            startWallet
                                        else do
                                            -- get VKeys of additional signers, in addition to the authenticated one
                                            liftIO $ promptUser putStrLn "  Enter VKeys of other signers: "
                                            let existingSigners = [fromJust maybeAuthenticatedVk]
                                            allSigners <- liftIO $ addSigner existingSigners (numPotentialSigners - 1)

                                            -- Now that we have we've collected parameters, create the proposed new Account
                                            let newAccount = Account accountName allSigners thresholdNum 100 []

                                            -- In the current design, no approval is needed to create the account. There's a known risk to usage is if it is created with bad Vks.
                                            -- currentTime <- liftIO getCurrentTime
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
                    liftIO $ warnUser putStrLn "Unexpected choice"
                    startWallet

authenticatePk :: Wallet -> IO Wallet
authenticatePk w = do
    promptUser putStr "To authenticate, enter your public key: "
    vk <- getLine
    promptUser putStr "Now, enter your private key: "
    sk <- getLine
    if isAuthenticatedPair vk sk
        then do
            putStrLn $ "Successfully authenticated as " ++ vk
            let newWallet = Wallet (ah_accounts w) (ah_activeAccountIndex w) (pure vk)
            pure newWallet
        else do
            warnUser putStrLn "Invalid match, unable to authenticate"
            pure w

addSigner :: [Vk] -> Int -> IO [Vk]
addSigner vks 0 = pure vks
addSigner vks numAddlSigners = do
    promptUser putStr "Public Key of additional signer: "
    vk <- getLine
    if vk `notElem` _TEST_Vks_
        then do
            warnUser putStrLn "Public key is not in known list. Try again."
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
                , (4, "Create Spend Request")
                , (5, "Endorse Pending Spend Request")
                , (9, "Return to Wallet")
                ]

listMenuOptions :: [(Int, String)] -> IO ()
listMenuOptions [] = pure ()
listMenuOptions (a : as) = do
    putChar $ intToDigit $ fst  a
    putStr ". "
    putStrLn $ snd a
    listMenuOptions as

startAccount :: (MonadIO m, MonadState Wallet m) => m ()
startAccount = do
    w <- get
    let maybeAccountIndex = ah_activeAccountIndex w
    case maybeAccountIndex of
        Nothing -> do
            liftIO $ warnUser putStrLn "Expected active account index to be set"
            pure ()
        Just accountIndex -> do
            -- TODO verify active user is set
            let activeAccount = ah_accounts w !! accountIndex
    
            liftIO $ emphasisUser putStrLn $ "ACCOUNT MODE   Account: " ++ a_accountId activeAccount ++ "  User: " ++ fromJust (ah_authenticatedVk w)
            liftIO $ listMenuOptions menuOptions
            liftIO $ promptUser putStr "Enter choice: "
            char <- liftIO getUpperChar
            case char of
                '1' -> do
                    -- Print Account
                    liftIO $ printAccount activeAccount
                    startAccount
                '2' -> do
                    -- Print All Txs
                    liftIO $ print $ a_spendTxs activeAccount
                    startAccount
                '3' -> do
                    -- Print Pending Txs
                    liftIO $ warnUser print "Not yet implemented"
                    startAccount
                '4' -> do
                    -- Create Spend Request
                    liftIO $ promptUser putStrLn "Input Spend Request parameters:"
                    liftIO $ promptUser putStr "  Recipient's public key: "
                    recipientVk <- liftIO getLine
                    if recipientVk `notElem` _TEST_Vks_
                        then do
                            liftIO $ warnUser putStrLn "Unknown public key"
                            startAccount
                        else do
                            liftIO $ promptUser putStr "  Amount as a whole positive number: "
                            amtString <- liftIO getLine
                            let amtInt = decimalStringToInt amtString
                            utcNow <- liftIO getCurrentTime 
                            let requestorVk = fromJust $ ah_authenticatedVk w
                            let eUpdatedAccount = addSpendRequestTx requestorVk recipientVk amtInt activeAccount utcNow
                            liftIO $ putStrLn "before: "
                            liftIO $ print activeAccount
                            case eUpdatedAccount of
                                Left ex -> do
                                    liftIO $ print ex
                                    startAccount
                                Right a -> do
                                    liftIO $ putStrLn "after: "
                                    liftIO $ print a

                                    -- replace the updated account in list of accounts
                                    let oldAccounts = ah_accounts w
                                    -- TODO move this into Lib
                                    let newAccounts = map (\aa -> if a_accountId a == a_accountId aa then a else aa) oldAccounts
                                    let newWallet = Wallet newAccounts (ah_activeAccountIndex w) (ah_authenticatedVk w)

                                    put newWallet
                                    startAccount
                '5' -> do
                    -- Endorse Pending Spend Tx
                    liftIO $ warnUser print "Not yet implemented"
                    startAccount
                '9' ->
                    -- Exit account. Return to wallet
                    pure ()
                _ -> do
                    liftIO $ warnUser print "Unexpected entry!"
                    startAccount
            pure () -- return to caller, e.g. wallet mode
            -- TODO verify updated wallet gets returned to caller