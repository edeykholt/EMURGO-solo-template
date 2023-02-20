module Types where

data AccountRequestTx = CreateTx | SpendTx | AddSignerTx | RemoveSignerTx | UpdateQuorumTx
    deriving (Eq, Ord, Show, Read)

-- verification key, or public key
type Vk = String

-- signing key, or private key
type Sk = String

data Wallet = Wallet {
   ah_accounts            :: [] Account
   }

data Account = Account {
    txs :: [] AccountRequestTx
    , signers :: [] Vk
    , requiredSigs :: Int
    , balance :: Int
}

data AccountTxVote = ApproveTxVote | RejectTxVote

-- ? are non-abstract versions of this required?
data AccountRequest = AccountRequest {
           r_txState :: AccountRequestState
           , r_createdDate :: String
           , r_lastUpdatedDate :: String
           , r_tx :: AccountRequestTx
           } -- deriving (Show)   

data AccountRequestState = RequestedWS | PendingWS | ApprovedWS | RejectedWS | ExpiredWS | ApprovedNsfWS | OtherErrorWS

data RequestException = NsfEx | UnauthorizedSignerEx | TimedOutEx | RedundantVoteEx | AlreadyFinalizedEx -- ??

-- Transaction time to live before expired, in seconds
_TxTTL_ = 600 



