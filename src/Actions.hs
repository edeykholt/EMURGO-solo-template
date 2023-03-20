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
import Text.ParserCombinators.ReadP (count)
import Data.Time (getCurrentTime)
import GHC.IO hiding (liftIO)
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
            evalStateT startWallet _TEST_WALLET_
        else do
            putStrLn "Initializing with empty wallet"
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

-- get first character of an input line
getUpperChar :: IO Char
getUpperChar = do
    x <- getLine
    case x of 
        [] -> pure 'z'
        c:_ -> pure (toUpper c)

startWallet :: (MonadIO m, MonadState Wallet m) => m ()
startWallet = do
    Wallet accounts maybeActiveAccountIndex maybeAuthenticatedVk <- get
    liftIO $ emphasisUser putStr "WALLET MODE   Authenticated User: " 
    if isNothing maybeAuthenticatedVk
        then do
            liftIO $ emphasisUser putStrLn "none"
            -- authenticate and restart
            newWallet <- liftIO $ authenticatePk $ Wallet accounts maybeActiveAccountIndex maybeAuthenticatedVk
            put newWallet
            startWallet
        else do
            liftIO $ emphasisUser putStrLn $ fromJust maybeAuthenticatedVk
            liftIO $ listAccounts accounts
            liftIO $ putStrLn "Commands..."
            liftIO $ putStrLn "1. List Accounts with Detail\n2. Enter Account Mode \n3. Add Account\n4. Authenticate \n9. Exit App"
            liftIO $ promptUser putStr "Enter choice: "
            selectedCmd <- liftIO getUpperChar
            case selectedCmd of
                '1' -> do
                    -- List Accounts
                    liftIO $ putStrLn $ unlines $ map prettyAccount accounts
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
                                    liftIO $ putStrLn . prettyAccount $ accounts !! selectedIdx
                                    let newWallet = Wallet accounts (Just selectedIdx) maybeAuthenticatedVk
                                    put newWallet
                                    newerWallet <- execStateT startAccount newWallet
                                    put newerWallet
                                    liftIO $ putStrLn $ prettyWallet newerWallet
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
                            
                            liftIO $ promptUser putStr "Account Name: "
                            accountName <- liftIO getLine
                            
                            liftIO $ promptUser putStr "Number of required signers: "
                            x <- liftIO getUpperChar
                            let thresholdNum = digitToInt x
                            let isInvalidThreshold = thresholdNum < 2 || thresholdNum > 9
                            if isInvalidThreshold
                                then do
                                    liftIO $ warnUser putStrLn "Invalid number of required signers"
                                    startWallet
                                else do
                                    liftIO $ promptUser putStr "Number of potential signers (including you): "
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
                                            
                                            -- Update the wallet with the new Account
                                            let newAccounts = accounts ++ [newAccount]
                                            let newIndex = length newAccounts - 1
                                            let newWallet = Wallet newAccounts (Just newIndex) maybeAuthenticatedVk
                                            put newWallet
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

-- authenticatePk is a currently weak way to validate a user is authentic. Intent is to replace this with cryptographic signing and validation later.
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
    -- For now, we are validating users by seeing if they are in the test list. For future, we'd verify the syntax of provided public key, address, etc.
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
            putStrLn $ prettyAccountsWithNum accounts

printAccount :: Account -> IO ()
printAccount a = do
    putStrLn $ prettyAccount a

menuOptions :: [(Int, String)]
menuOptions = [(1, "Print Account")
                , (2, "Print All Send Requests")
                , (3, "Print Pending Send Requests")
                , (4, "Create Send Request")
                , (5, "Endorse Pending Send Request")
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
        Just accountIndex -> do
            let maybeAuthenticatedVk = ah_authenticatedVk w
            case maybeAuthenticatedVk of
                Nothing ->
                    liftIO $ warnUser putStrLn "no user authenticated"
                Just authenticatedUser -> do
                    let activeAccount = ah_accounts w !! accountIndex
                    liftIO $ emphasisUser putStrLn $ "ACCOUNT MODE   Account: " ++ a_accountId activeAccount ++ "  User: " ++ fromJust (ah_authenticatedVk w)
                    liftIO $ listMenuOptions menuOptions
                    liftIO $ promptUser putStr "Enter choice: "
                    char <- liftIO getUpperChar
                    case char of
                        '1' -> do
                            -- Print Account
                            liftIO $ putStrLn $ prettyAccount activeAccount
                            startAccount
                        '2' -> do
                            -- Print All Send Requests
                            liftIO $ putStrLn $ prettyRequests (a_sendTxs activeAccount)
                            startAccount
                        '3' -> do
                            -- Print Pending Send Requests
                            liftIO $ warnUser print "Not yet implemented"
                            startAccount
                        '4' -> do
                            -- Create Send Request
                            liftIO $ promptUser putStrLn "Input Send Request parameters:"
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
                                    let eUpdatedAccount = addSendRequestTx requestorVk recipientVk amtInt activeAccount utcNow
                                    liftIO $ putStrLn "before: "
                                    liftIO $ print activeAccount
                                    case eUpdatedAccount of
                                        Left ex -> do
                                            liftIO $ print ex
                                            startAccount
                                        Right a -> do
                                            liftIO $ putStrLn "after: "
                                            liftIO $ print a
                                            liftIO $ putStrLn $ prettyAccount a
                                            let newWallet = replaceAccount w a
                                            case newWallet of
                                                Right nw -> do
                                                    put nw
                                                    startAccount
                                                Left _ -> do
                                                    liftIO $ warnUser putStrLn "Could not replace account in wallet"
                                                    startAccount
                        '5' -> do
                            -- Endorse Pending Send Request
                            liftIO $ promptUser putStr "Request#: "
                            idxString <- liftIO getLine
                            let idx = decimalStringToInt idxString
                            
                            if idx < 0 || idx + 1 > length (a_sendTxs activeAccount)
                                then do
                                    liftIO $ warnUser putStrLn "Index out of range"
                                    startAccount
                                else do
                                    let availableRequests = a_sendTxs activeAccount
                                    let selectedRequest = availableRequests !! idx
                                    utcNow <- liftIO getCurrentTime 
                                    let endorsement = AccountTxVoteTx {
                                        atxv_txId="ignored"
                                        , atxv_dateTime = show utcNow  -- TODO strengthen type
                                        , atxv_approverVk= authenticatedUser
                                        , atxv_accountId= a_accountId activeAccount
                                    }
                                    let updatedAccountAndSR = applyEndorsement activeAccount selectedRequest endorsement
                                    
                                    case updatedAccountAndSR of
                                        Left ex -> do
                                            -- TODO print the ex
                                            liftIO $ warnUser putStrLn "Could not apply endorsement"
                                            startAccount
                                        Right (updatedAcct, updatedSendRequest) -> do
                                            let eNewWallet = replaceAccount w updatedAcct
                                            case eNewWallet of
                                                Left ex -> do
                                                    liftIO $ print ex
                                                    liftIO $ warnUser putStrLn "Could not replace account in wallet"
                                                    startAccount
                                                Right newWallet -> do
                                                    put newWallet
                                                    liftIO $ putStrLn "Endorsed. Updated Send Transaction: "
                                                    liftIO $ putStrLn $ prettyRequest updatedSendRequest
                                                    liftIO $ putStrLn $ "Account balance: " ++ show (a_balance updatedAcct)
                                                    startAccount
                        '9' -> do
                            -- Exit account. Return to wallet
                            pure ()
                        _ -> do
                            liftIO $ warnUser print "Unexpected entry!"
                            startAccount
                    pure () -- return to caller, e.g. wallet mode
