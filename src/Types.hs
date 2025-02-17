{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Types where
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
_TEST_Vks_ = [_TEST_ALICE_VK_, _TEST_BOB_VK_, _TEST_CAROL_VK_, _TEST_DAN_VK_]

-- signing key, or private key
type Sk = String
_TEST_ALICE_SK_ = "AAA"
_TEST_BOB_SK_ = "BBB"
_TEST_CAROL_SK_ = "CCC"
_TEST_DAN_SK_ = "DDD"
_TEST_Sks_ = [_TEST_ALICE_SK_ , _TEST_BOB_SK_ , _TEST_CAROL_SK_ , _TEST_DAN_SK_ ]

-- Account Request Transactions, below, go through a number of potential states until finalized
data TxState = TxPendingEndorsement | TxApproved | TxApprovedNsf
    deriving (Show, Eq)

-- Exceptions that may be returned on attempted Wallet updates
data WalletException = WalletUpdateException | AccountSignersNotUnique
    deriving Show

-- Couldn't find an easier function such as (String -> UTCTime).  TODO: Consider in the future:   read :: String -> UTCTime
mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
    UTCTime (fromGregorian year mon day) (sinceMidnight (TimeOfDay hour min sec))
_TEST_UTCTime_ = mkUTCTime (2023, 01, 01) (01, 0, 0)

-- Varoius kinds of Account Request Transactions share the same base set of property types
data AccountRequestTxBase = AccountRequestTxBase {
    btx_accountId             :: String  -- TODO redundant?  Friendly Name?
    , btx_txId                :: String
    , btx_txCreator           :: Vk
    , btx_createdDateTime     :: UTCTime
    , btx_txState             :: TxState
    , btx_endorsementTxs      :: [] EndorsementTx
} deriving Show
_TEST_ARTxBase_ = AccountRequestTxBase "Test Account" "txId" _TEST_ALICE_VK_ _TEST_UTCTime_ TxPendingEndorsement []

-- Request to send value
data SendRequestTx = SendRequestTx {
    stx_base            :: AccountRequestTxBase
    , stx_recipient     :: Vk
    , stx_sendAmount    :: Int
} deriving Show
_TEST_SendRequestTx_ = SendRequestTx _TEST_ARTxBase_ _TEST_BOB_VK_ 10

data Account = Account {
    a_accountId      :: String
    , a_signers      :: [] Vk
    , a_requiredSigs :: Int
    , a_balance      :: Int
    , a_sendTxs      :: [] SendRequestTx
} deriving Show
_TEST_ACCOUNT_ = Account "Test Account" [_TEST_ALICE_VK_, _TEST_BOB_VK_, _TEST_CAROL_VK_ ] 2 100 [_TEST_SendRequestTx_]

data Wallet = Wallet {
   ah_accounts              :: [] Account
   , ah_activeAccountIndex  :: Maybe Int
   , ah_authenticatedVk     :: Maybe Vk
   } deriving Show
_TEST_WALLET_ = Wallet [_TEST_ACCOUNT_] Nothing Nothing -- initial account is active, no user authenticated

-- Used to endorse a SendRequest and potentially others in the future
data EndorsementTx = EndorsementTx {
    atxv_accountId           :: String
    ,atxv_sendTxDateTime     :: UTCTime
    ,atxv_approverVk         :: Vk
    ,atxv_dateTime           :: UTCTime
} deriving Show
_TEST_AccountTxVoteTx_ = EndorsementTx {
    atxv_accountId = a_accountId _TEST_ACCOUNT_
    , atxv_sendTxDateTime = _TEST_UTCTime_
    , atxv_approverVk = _TEST_BOB_VK_
    , atxv_dateTime = read "2023-01-02 00:00:00 +0000"
    }

-- Exceptions for SendRequestTx or EndorsementTx
data RequestException = NsfEx | UnauthorizedSignerEx | TimedOutEx | RedundantVoteEx | AlreadyFinalizedEx | EndorsementTargetNotFoundEx | OtherEx
    -- deriving Show