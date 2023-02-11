module Types where

data WalletTx = SpendTx | AddSignerTx | RemoveSignerTx | UpdateQuorumTx
    deriving (Eq, Ord, Show, Read)

-- verification, or public key
type Vk = String
-- signing, or private key
type Sk = String

newtype AppHost = AppHost {
    ah_authenticatedUsers :: [] String
}

data WalletVoteTx = ApproveTx | RejectTx | UndecidedTx



data Request = Request {
    r_txState :: WalletState
    , r_createdDate :: String
    , r_lastUpdatedDate :: String
    , r_tx :: WalletTx
    } -- deriving (Show)   

data Wallet = Wallet {
    txs :: [] WalletTx
}

data WalletState = RequestedWS | PendingWS | ApprovedWS | RejectedWS | ExpiredWS | ApprovedNsfWS | OtherErrorWS

data WalletException = NsfEx | UnauthorizedSignerEx | TimedOutEx | RedundantVoteEx | AlreadyFinalizedEx -- ??

-- Transaction time to live before expired, in seconds
_TxTTL_ = 600 



