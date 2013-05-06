{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- | An interface to bitcoind's available network-related RPC calls.
--   The implementation of these functions can be found at
--   <https://github.com/bitcoin/bitcoin/blob/master/src/rpcnet.cpp>.
--
--   If any APIs are missing, patches are always welcome. If you look at the
--   source of this module, you'll see that the interface code is trivial.
module Network.Bitcoin.Net ( Auth(..)
                           , getConnectionCount
                           , PeerInfo(..)
                           , getPeerInfo
                           ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Network.Bitcoin.Internal

-- | Returns the number of connections to other nodes.
getConnectionCount :: Auth -> IO Integer
getConnectionCount auth = callApi auth "getconnectioncount" []

-- | Information about a peer node of the Bitcoin network.
--
--   The documentation for this data structure is incomplete, as I honestly
--   don't know what some of these fields are for. Patches are welcome!
data PeerInfo =
    PeerInfo { -- | The IP:port of this peer, as a string.
               addressName :: Text
             , services :: Text
             -- | Relative to the first time we conected with this peer (and in
             -- milliseconds), the last time we sent this peer any data.
             , lastSend :: Integer
             -- | Relative to the first time we connected with this peer
             --   (and in milliseconds), the last time we sent this peer any
             --   data.
             , lastRecv :: Integer
              -- | How long have we been connected to this peer (in
              --   milliseconds).
             , connectionTime :: Integer
             -- | The version of the Bitcion client the peer is running.
             , peerVersion :: Integer
             -- | The sub-version of the Bitcoin client the peer is running.
             , peerSubversion :: Text
             , inbound :: Bool
             , releaseTime :: Integer
             , startingHeight :: Integer
             -- | How many times has this peer behaved badly?
             , banScore :: Integer
             }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON PeerInfo where
    parseJSON (Object o) = PeerInfo <$> o .: "addr"
                                    <*> o .: "services"
                                    <*> o .: "lastsend"
                                    <*> o .: "lastrecv"
                                    <*> o .: "conntime"
                                    <*> o .: "version"
                                    <*> o .: "subver"
                                    <*> o .: "inbound"
                                    <*> o .: "releasetime"
                                    <*> o .: "startingheight"
                                    <*> o .: "banscore"
    parseJSON _ = mzero

-- | Returns data about all connected peer nodes.
getPeerInfo :: Auth -> IO [PeerInfo]
getPeerInfo auth = callApi auth "getpeerinfo" []
