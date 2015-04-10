{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- | The API exposed in this module should be considered unstable, and is
--   subject to change between minor revisions.
--
--   If the version number is a.b.c.d, and either a or b changes, then the
--   module's whole API may have changed (if only b changes, then it was
--   probably a minor change).
--
--   If c changed, then only the internal API may change. The rest of the
--   module is guaranteed to be stable.
--
--   If only d changes, then there were no user-facing code changes made.
module Network.Bitcoin.Internal ( module Network.Bitcoin.Types
                                , Text, Vector
                                , FromJSON(..)
                                , callApi
                                , getClient
                                , Nil(..)
                                , NilOrArray(..)
                                , tj
                                , tjm
                                , tja
                                , AddrAddress(..)
                                , BitcoinRpcResponse(..)
                                ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           Data.Vector ( Vector )
import qualified Data.Vector          as V
import           Network.Bitcoin.Types
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Text ( Text )
import           Network.HTTP.Client
import           Network.HTTP.Types.Header


-- | RPC calls return an error object. It can either be empty; or have an
--   error message + error code.
data BitcoinRpcError = NoError -- ^ All good.
                     | BitcoinRpcError Int Text -- ^ Error code + error message.
    deriving ( Show, Read, Ord, Eq )

instance FromJSON BitcoinRpcError where
    parseJSON (Object v) = BitcoinRpcError <$> v .: "code"
                                           <*> v .: "message"
    parseJSON Null       = return NoError
    parseJSON _ = mzero

-- | A response from bitcoind will contain the result of the JSON-RPC call, and
--   an error. The error should be null if a valid response was received.
data BitcoinRpcResponse a = BitcoinRpcResponse { btcResult  :: a
                                               , btcError   :: BitcoinRpcError
                                               }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON a => FromJSON (BitcoinRpcResponse a) where
    parseJSON (Object v) = BitcoinRpcResponse <$> v .: "result"
                                              <*> v .: "error"
    parseJSON _          = mzero

-- | 'getClient' takes a url, rpc username, and rpc password
--   and returns a Client that can be used to make API calls. Each
--   Client encloses a Manager (from http-client) that re-uses
--   connections for requests, so long as the same Client is
--   is used for each call.
getClient :: String
          -> BS.ByteString
          -> BS.ByteString
          -> IO Client
getClient url user pass = do
    url' <- parseUrl url
    mgr <- newManager defaultManagerSettings
    let baseReq = applyBasicAuth user pass url'
            { method = "POST"
            , requestHeaders = [(hContentType, "application/json")] }
    return $ \r -> do
        resp <- httpLbs (baseReq { requestBody = RequestBodyLBS r }) mgr
        return $ responseBody resp

-- | 'callApi' is a low-level interface for making authenticated API
--   calls to a Bitcoin daemon. The first argument specifies
--   rpc client details (URL, username, password)
--
--   The second argument is the command name.  The third argument provides
--   parameters for the API call.
--
--   > genHash = do
--       client <- getClient "http://127.0.0.1:8332" "user" "password"
--       callApi client "getblockhash" [tj 0]
--
--   On error, throws a 'BitcoinException'.
callApi :: FromJSON v
        => Client  -- ^ RPC client for bitcoind
        -> Text    -- ^ command name
        -> [Value] -- ^ command arguments
        -> IO v
callApi client cmd params = readVal =<< client jsonRpcReqBody
    where
        readVal bs = case eitherDecode' bs of
                         Right r@(BitcoinRpcResponse {btcError=NoError})
                             -> return $ btcResult r
                         Right (BitcoinRpcResponse {btcError=BitcoinRpcError code msg})
                             -> throw $ BitcoinApiError code msg
                         Left err
                             -> throw $ BitcoinResultTypeError (BSL8.pack err)

        jsonRpcReqBody =
            encode $ object [ "jsonrpc" .= ("2.0" :: Text)
                            , "method"  .= cmd
                            , "params"  .= params
                            , "id"      .= (1 :: Int)
                            ]
{-# INLINE callApi #-}

-- | Used to allow "null" to decode to a tuple.
data Nil = Nil { unNil :: () }

instance FromJSON Nil where
    parseJSON Null = return $ Nil ()
    parseJSON x    = fail $ "\"null\" was expected, but " ++ show x ++ " was recieved."

-- | Used to parse "null" or [HexString]
data NilOrArray = NilOrArray {unArr :: Maybe [HexString]}

instance FromJSON NilOrArray where
    parseJSON Null = return $ NilOrArray Nothing
    parseJSON a@(Array _) = liftM NilOrArray $ parseJSON a
    parseJSON x = fail $ "Expected \"null\" or array, but " ++ show x ++ " was recieved."

-- | A handy shortcut for toJSON, because I'm lazy.
tj :: ToJSON a => a -> Value
tj = toJSON
{-# INLINE tj #-}

tjm :: ToJSON a => a -> Maybe a -> Value
tjm d m = tj $ fromMaybe d m
{-# INLINE tjm #-}

tja :: ToJSON a => Maybe a -> [Value]
tja m = fromMaybe [] $ pure . tj <$> m
{-# INLINE tja #-}

-- | A wrapper for a vector of address:amount pairs. The RPC expects that as
--   an object of "address":"amount" pairs, instead of a vector. So that's what
--   we give them with AddrAddress's ToJSON.
newtype AddrAddress = AA (Vector (Address, BTC))

instance ToJSON AddrAddress where
    toJSON (AA vec) = object . V.toList $ uncurry (.=) <$> vec
