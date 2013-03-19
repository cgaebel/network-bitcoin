{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- | An interface to bitcoind's available block-chain-related RPC calls. The
--   implementation of these functions can be found at
--   <https://github.com/bitcoin/bitcoin/blob/master/src/rpcblockchain.cpp>.
--
--   If any APIs are missing, patches are always welcome. If you look at the
--   source of this module, you'll see that the interface code is trivial.
module Network.Bitcoin.BlockChain ( Auth(..)
                                  , TransactionID
                                  , BTC
                                  , ScriptSig(..)
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
                                  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Network.Bitcoin.Internal

-- | Returns the number of blocks in the longest block chain.
getBlockCount :: Auth -> IO Integer
getBlockCount auth = callApi auth "getblockcount" []

-- | Returns the proof-of-work difficulty as a multiple of the minimum
--   difficulty.
getDifficulty :: Auth -> IO Integer
getDifficulty auth = callApi auth "getdifficulty" []

-- | Sets the transaction fee will will pay to the network. Values of 0 are
--   rejected.
setTransactionFee :: Auth -> BTC -> IO ()
setTransactionFee auth fee =
    stupidAPI <$> callApi auth "settxfee" [ tj fee ]
        where stupidAPI :: Bool -> ()
              stupidAPI = const ()

-- | Returns all transaction identifiers in the memory pool.
getRawMemoryPool :: Auth -> IO (Vector TransactionID)
getRawMemoryPool auth = callApi auth "getrawmempool" []

-- | The hash of a given block.
type BlockHash = HexString

-- | Returns the hash of the block in best-block-chain at the given index.
getBlockHash :: Auth
             -> Integer -- ^ Block index.
             -> IO BlockHash
getBlockHash auth idx = callApi auth "getblockhash" [ tj idx ]

-- | Information about a given block in the block chain.
data Block = Block { blockHash :: BlockHash
                   -- | The number of confirmations the block has.
                   , blkConfirmations :: Integer
                   -- | The size of the block.
                   , blkSize :: Integer
                   -- | The "height" of the block. TODO: Clarify this.
                   , blkHeight :: Integer
                   -- | The version of the block.
                   , blkVersion :: Integer
                   -- | The hash of the block at the root of the merkle tree
                   --   which this block belongs to.
                   , merkleRoot :: BlockHash
                   -- | Should this be a transaction, or transaction id?
                   , subTransactions :: Vector TransactionID
                   -- | The time it was mined.
                   , blkTime :: Integer
                   -- | The block's nonce.
                   , blkNonce :: Integer
                   , blkBits :: HexString
                   -- | How hard was this block to mine?
                   , blkDifficulty :: Integer
                   -- | A pointer to the next block in the chain.
                   , nextBlock :: Maybe BlockHash
                   -- | A pointer to the previous block in the chain.
                   , prevBlock :: Maybe BlockHash
                   }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON Block where
    parseJSON (Object o) = Block <$> o .:  "hash"
                                 <*> o .:  "confirmations"
                                 <*> o .:  "size"
                                 <*> o .:  "height"
                                 <*> o .:  "version"
                                 <*> o .:  "merkleroot"
                                 <*> o .:  "tx"
                                 <*> o .:  "time"
                                 <*> o .:  "nonce"
                                 <*> o .:  "bits"
                                 <*> o .:  "difficulty"
                                 <*> o .:? "nextblockhash"
                                 <*> o .:? "previousblockhash"
    parseJSON _ = mzero

-- | Returns details of a block with given block-hash.
getBlock :: Auth -> BlockHash -> IO Block
getBlock auth bh = callApi auth "getblock" [ tj bh ]

-- | Information on the unspent transaction in the output set.
data OutputSetInfo =
    OutputSetInfo { osiBestBlock :: BlockHash
                  -- | The number of transactions in the output set.
                  , numTransactions :: Integer
                  -- | The number of outputs for the transactions.
                  , transactionOutputs :: Integer
                  -- | The serialized size of the output set.
                  , serializedSize :: Integer
                  }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON OutputSetInfo where
    parseJSON (Object o) = OutputSetInfo <$> o .: "bestblock"
                                         <*> o .: "transactions"
                                         <*> o .: "txouts"
                                         <*> o .: "bytes_serialized"
    parseJSON _ = mzero

-- | Returns statistics about the unspent transaction output set.
getOutputSetInfo :: Auth -> IO OutputSetInfo
getOutputSetInfo auth = callApi auth "gettxoutsetinfo" []

-- | Details about an unspent transaction output.
data OutputInfo =
    OutputInfo { oiBestBlock :: BlockHash
               -- | The number of times this transaction has been confirmed.
               , oiConfirmations :: Integer
               -- | The amount transferred.
               , oiAmount :: BTC
               -- | The public key of the sender.
               , oiScriptPubKey :: ScriptSig
               -- | The version of this transaction.
               , oiVersion :: Integer
               -- | Is this transaction part of the coin base?
               , oiCoinBase :: Bool
               }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON OutputInfo where
    parseJSON (Object o) = OutputInfo <$> o .: "bestblock"
                                      <*> o .: "confirmations"
                                      <*> o .: "amount"
                                      <*> o .: "scriptPubKey"
                                      <*> o .: "version"
                                      <*> o .: "coinbase"
    parseJSON _ = mzero

-- | Returns details about an unspent transaction output.
getOutputInfo :: Auth
              -> TransactionID
              -> Integer -- ^ The index we're looking at.
              -> IO OutputInfo
getOutputInfo auth txid n = callApi auth "gettxout" [ tj txid, tj n ]
