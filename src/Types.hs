{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Types where
    
import GHC.Natural (Natural)
import Data.Fixed 
import Data.Time
import Text.Show.Functions

-- verification key, or public key
type Vk = String
_TEST_VK_ = "1234"

-- signing key, or private key
type Sk = String
_TEST_SK_ = "asdf"

-- Account Request Transactions, below, go through a number of potential states resolved as Expired or Approved
data TxState = RequestedTx | PendingTx | ApprovedTx | RejectedTx | ExpiredTx | ApprovedNsfTx | OtherErrorTx
    deriving Show

data WalletException = WalletException1 | WalletException2
    deriving Show

data AccountException = AccountException1 | AccountException2
    deriving Show

-- Varoius kinds of Account Request Transactions share the same base set of property types
data AccountRequestTxBase = AccountRequestTxBase {
    btx_expirationInSeconds     :: Natural
    , btx_accountId             :: String
    , btx_txId                  :: String
    , btx_txCreator             :: Vk
    , btx_txSignature           :: String
    , btx_createdDateTime       :: UTCTime
    , btx_txState               :: TxState
    , btx_isFinalized           :: Bool
    , btx_voteTxs               :: [] AccountTxVoteTx
} deriving Show


mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
    UTCTime (fromGregorian year mon day) (sinceMidnight (TimeOfDay hour min sec))
_TEST_UTCTime_ = mkUTCTime (2023, 01, 01) (01, 0, 0)

_TEST_ARTxBase_ = AccountRequestTxBase 100 "accountId" "txId" _TEST_VK_ "sig" _TEST_UTCTime_  RequestedTx False []

-- Request to create a new multisig account
data CreateTx = CreateTx {
    ctx_base                     :: AccountRequestTxBase
    , ctx_additionalSigners      :: [] Vk
    , ctx_approvalThreshold      :: Natural
} deriving Show

_TEST_CreateTx_ = CreateTx _TEST_ARTxBase_ [_TEST_VK_, _TEST_VK_] 2

-- Request to spend
data SpendTx = SpendTx {
    stx_base             :: AccountRequestTxBase
    , stx_spendAmount    :: Natural
} deriving Show
_TEST_SpendTx_ = SpendTx _TEST_ARTxBase_ 10

-- Request to add a signer
data AddSignerTx = AddSignerTx {
    astx_base            :: AccountRequestTxBase
    , astx_signer        :: Vk
} deriving Show
_TEST_AddSignerTx_ = AddSignerTx _TEST_ARTxBase_ _TEST_VK_

-- Request to remove a signer
data RemoveSignerTx = RemoveSignerTx {
    rstx_base            :: AccountRequestTxBase
    , rstx_signer        :: Vk
} deriving Show
_TEST_RemoveSignerTx_ = RemoveSignerTx _TEST_ARTxBase_ _TEST_VK_

-- Request to change the approval threshold, i.e., number of signers
data UpdateNumSignersTx = UpdateNumSignersTx {
    untx_base    :: AccountRequestTxBase
    , untx_newThreshold :: Natural
} deriving Show
_TEST_UpdateNumSignerTx_ = UpdateNumSignersTx _TEST_ARTxBase_ 3

-- consider using dynamicCast type witnesses?

data Account = Account {
    a_accountId :: String
    , a_signers :: [] Vk
    , a_requiredSigs :: Int
    , a_balance :: Int
    , a_createTx :: CreateTx
    , a_spendTxs :: [] SpendTx
    , a_addSignerTxs :: [] AddSignerTx
    , a_removeSignerTxs :: [] RemoveSignerTx
    , s_updateNumSignersTxs :: [] UpdateNumSignersTx
} deriving Show
_TEST_ACCOUNT_ = Account "accountId" [_TEST_VK_, _TEST_VK_ ] 2 100 _TEST_CreateTx_  [] [] [] []

data Wallet = Wallet {
   ah_accounts              :: [] Account
   , ah_activeAccountIndex  :: Int
   } deriving Show
_TEST_WALLET_ = Wallet [_TEST_ACCOUNT_] 0 -- initial account is active

data AccountTxVoteTx = AccountTxVoteTx {
    atxv_accountId     :: String
    ,atxv_txId         :: String
    ,atxv_approverVk   :: Vk
    ,atxv_dateTime      :: String
    ,atxv_isApproved   :: Bool
} deriving Show
_TEST_AccountTxVoteTx_ = AccountTxVoteTx "qwery" "asdf" _TEST_VK_ "2023-01-02" True

data RequestException = NsfEx | UnauthorizedSignerEx | TimedOutEx | RedundantVoteEx | AlreadyFinalizedEx

-- Transaction time to live before expired, in seconds
_TxTTL_ = 600 
