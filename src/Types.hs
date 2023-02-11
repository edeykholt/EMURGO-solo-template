module Types where

data WalletTx = SpendTx | AddSignerTx | RemoveSignerTx | UpdateQuorumTx
    deriving (Eq, Ord, Show, Read)

-- verification, or public key
type Vk = string
-- signing, or private key
type Sk = string

datatype AppHost = AppHost {
    authenticatedUsers = [] Vk
    }


data WalletVoteTx = ApproveTx | RejectTx | UndecidedTx

datatype Request = Request {
    r_walletState = string
    , r_createdDate = string
    , r_lastUpdatedDate = string
    , r_txs = [] Tx
}

datatype Wallet = Wallet {
    txs = [] Tx
}

data WalletState = RequestedWS | PendingWS | ApprovedWS | RejectedWS | ExpiredWS | ApprovedNsfWS | OtherErrorWS

data WalletExceptions = NsfEx | UnauthorizedSignerEx | TimedOutEx | RedundantVoteEx | AlreadyFinalizedEx -- ??

-- Transaction time to live before expired, in seconds
_TxTTL_ = 600 



