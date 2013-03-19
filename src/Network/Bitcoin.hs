{-# OPTIONS_GHC -Wall #-}
-- | A Haskell binding to the bitcoind server.
module Network.Bitcoin
    (
    -- * Common Types
      Auth(..)
    , BitcoinException(..)
    , HexString
    , TransactionID
    , Satoshi(..)
    , BTC
    , Account
    , Address
    , ScriptSig
    -- * Block Chain Operations
    , getBlockCount
    , getDifficulty
    , setTransactionFee
    , getRawMemoryPool
    , BlockHash
    , getBlockHash
    , Block(..)
    , getBlock
    , OutputSetInfo(..)
    , getOutputSetInfo
    , OutputInfo(..)
    , getOutputInfo
    -- * Private Key Operations
    , importPrivateKey
    , dumpPrivateKey
    -- * Mining Operations
    , getGenerate
    , setGenerate
    , getHashesPerSec
    , MiningInfo(..)
    , getMiningInfo
    , HashData(..)
    , getWork
    , solveBlock
    , Transaction(..)
    , CoinBaseAux(..)
    , BlockTemplate(..)
    , getBlockTemplate
    , submitBlock
    -- * Network Operations
    , getConnectionCount
    , PeerInfo(..)
    , getPeerInfo
    -- * Raw Transaction Operations
    , RawTransaction
    , getRawTransaction
    , TxIn(..)
    , TxnOutputType(..)
    , ScriptPubKey(..)
    , TxOut(..)
    , BlockInfo(..)
    , RawTransactionInfo(..)
    , getRawTransactionInfo
    , UnspentTransaction(..)
    , listUnspent
    , createRawTransaction
    , DecodedRawTransaction(..)
    , decodeRawTransaction
    , WhoCanPay(..)
    , RawSignedTransaction(..)
    , signRawTransaction
    , sendRawTransaction
    -- * Wallet Operations
    , BitcoindInfo(..)
    , getBitcoindInfo
    , getNewAddress
    , getAccountAddress
    , getAccount
    , setAccount
    , getAddressesByAccount
    , sendToAddress
    , AddressInfo(..)
    , listAddressGroupings
    , Signature
    , signMessage
    , verifyMessage
    , getReceivedByAddress
    , getReceivedByAddress'
    , getReceivedByAccount
    , getReceivedByAccount'
    , getBalance
    , getBalance'
    , getBalance''
    , moveBitcoins
    , sendFromAccount
    , sendMany
    -- , createMultiSig
    , ReceivedByAddress(..)
    , listReceivedByAddress
    , listReceivedByAddress'
    , ReceivedByAccount(..)
    , listReceivedByAccount
    , listReceivedByAccount'
    -- , listTransactions
    -- , listAccounts
    -- , listSinceBlock
    -- , getTransaction
    , backupWallet
    , keyPoolRefill
    , unlockWallet
    , lockWallet
    , changePassword
    , encryptWallet
    , isAddressValid
    ) where

import Network.Bitcoin.Types
import Network.Bitcoin.BlockChain
import Network.Bitcoin.Dump
import Network.Bitcoin.Mining
import Network.Bitcoin.Net
import Network.Bitcoin.RawTransaction
import Network.Bitcoin.Wallet
