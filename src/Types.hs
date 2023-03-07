{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Types where
    
import GHC.Natural (Natural)
import Data.Fixed 
import Data.Time
import Text.Show.Functions
import Data.Time.Format.ISO8601 (yearFormat)

-- verification key, or public key
type Vk = String
_TEST_ALICE_VK_ = "Alice"
_TEST_BOB_VK_ = "Bob"
_TEST_CAROL_VK_ = "Carol"
_TEST_DAN_VK_ = "Dan"

-- signing key, or private key
type Sk = String
_TEST_ALICE_SK_ = "AAA"
_TEST_BOB_SK_ = "BBB"
_TEST_CAROL_SK_ = "CCC"
_TEST_DAN_SK_ = "DDD"

-- alternation-based ad-hoc polymorphism
-- data RequestTx =
    -- | SpendRequestTx Int
    -- | AddRemoveSignerRequestTx Int
    -- | UpdateThresholdRequestTx Int

-- Account Request Transactions, below, go through a number of potential states resolved as Expired or Approved
data TxState = TxRequested | TxPending | TxApproved | TxRejected | TxExpired | TxApprovedNsf | TxOtherError
    deriving Show

data WalletException = WalletException1 | WalletException2
    deriving Show

data AccountException = AccountException1 | AccountException2
    deriving Show

-- TODO is there a simpler time function?
mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
    UTCTime (fromGregorian year mon day) (sinceMidnight (TimeOfDay hour min sec))
_TEST_UTCTime_ = mkUTCTime (2023, 01, 01) (01, 0, 0)

-- Varoius kinds of Account Request Transactions share the same base set of property types
data AccountRequestTxBase = AccountRequestTxBase {
    btx_expirationInSeconds     :: Natural
    , btx_accountId             :: String  -- redundant?  Friendly Name?
    , btx_txId                  :: String
    , btx_txCreator             :: Vk
    , btx_txSignature           :: String
    , btx_createdDateTime       :: UTCTime
    , btx_txState               :: TxState
    , btx_isFinalized           :: Bool
    , btx_voteTxs               :: [] AccountTxVoteTx
} deriving Show
_TEST_ARTxBase_ = AccountRequestTxBase 100 "accountId1" "txId" _TEST_ALICE_VK_ "sig" _TEST_UTCTime_ TxRequested False []

-- Request to create a new multisig account
data CreateAccountTx = CreateAccountTx {
    ctx_base                     :: AccountRequestTxBase
    -- required additional signers
    , ctx_additionalSigners      :: [] Vk  -- authorized endorsers
    , ctx_approvalThreshold      :: Natural
} deriving Show
_TEST_CreateAccountTx_ = CreateAccountTx _TEST_ARTxBase_ [_TEST_ALICE_VK_, _TEST_BOB_VK_] 2

-- Request to spend
data SpendRequestTx = SpendRequestTx {
    stx_base             :: AccountRequestTxBase
    , stx_spendAmount    :: Natural
} deriving Show
_TEST_SpendRequestTx_ = SpendRequestTx _TEST_ARTxBase_ 10

-- Request to add a signer
data AddSignerRequestTx = AddSignerRequestTx {
    astx_base            :: AccountRequestTxBase
    , astx_signer        :: Vk
} deriving Show
_TEST_AddSignerRequestTx_ = AddSignerRequestTx _TEST_ARTxBase_ _TEST_CAROL_VK_

-- Request to remove a signer
data RemoveSignerRequestTx = RemoveSignerRequestTx {
    rstx_base            :: AccountRequestTxBase
    , rstx_signer        :: Vk
} deriving Show
_TEST_RemoveSignerRequestTx_ = RemoveSignerRequestTx _TEST_ARTxBase_ _TEST_CAROL_VK_

-- Request to change the approval threshold, i.e., number of signers
data UpdateNumSignersRequestTx = UpdateNumSignersRequestTx {
    untx_base    :: AccountRequestTxBase
    , untx_newThreshold :: Natural
} deriving Show
_TEST_UpdateNumSignerRequestTx_ = UpdateNumSignersRequestTx _TEST_ARTxBase_ 3

-- consider using dynamicCast type witnesses?

data Account = Account {
    a_accountId :: String
    , a_signers :: [] Vk
    , a_requiredSigs :: Int
    , a_balance :: Int
    , a_createAccountTx :: CreateAccountTx
    , a_spendTxs :: [] SpendRequestTx
    , a_addSignerRequestTxs :: [] AddSignerRequestTx
    , a_removeSignerRequestTxs :: [] RemoveSignerRequestTx
    , s_updateNumSignersRequestTxs :: [] UpdateNumSignersRequestTx
} deriving Show
_TEST_ACCOUNT_ = Account "accountId" [_TEST_ALICE_VK_, _TEST_BOB_VK_ ] 2 100 _TEST_CreateAccountTx_  [] [] [] []

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
_TEST_AccountTxVoteTx_ = AccountTxVoteTx "qwery" "asdf" _TEST_BOB_VK_ "2023-01-02" True

data RequestException = NsfEx | UnauthorizedSignerEx | TimedOutEx | RedundantVoteEx | AlreadyFinalizedEx

-- Transaction time to live before expired, in seconds
_TxTTL_ = 600 
