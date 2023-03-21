# Command-Line Multisignature Wallet POC
# Ed Eykholt’s Haskell Project for - Emurgo Academy course - Haskell Programming Batch 81

# Overview
- This wallet creates some of the types interactions for a multi-sig wallet application, similar to Gnosis on Ethereum.  It is far from feature complete or an MVP.

# Concepts and Types, as implemeted
- Here, a Wallet is a list of Accounts. Each Account is named, has an amount balance, and a list of transactions.
- Each account starts out with 100 units of value in its balance.
- Users are authenticated with a key-pair.  Users are known by their public keys. key-pairs are simple strings in this project, without any cryptography.
- A SendRequest transaction is a request to transver value to another public key (Vk)
- SendRequests must be endorsed by the specified other users in order for a SendRequest transaction to be finalized and send value

# Highlights of user interaction:
- `runApp`
    - This is the function to start the app and enter the Wallet mode
    - Can optionally start with sample data
- Wallet Mode
    - Allows the user to authenticate, list configured accounts, create a new account, and enter Account Mode
- Account Mode
    - Allows the user to see the details of the account, add a SendRequest, and add an endorsement to an existing SendRequest

# Implementation Notes
- The app keeps track of the status of each account balances, SendRequests, and their summary states.  This is the core logic of the application beyond maintaining the data structures and Ux
- The Actions module uses use State and IO monads and stl library.
- There is no persistence.


