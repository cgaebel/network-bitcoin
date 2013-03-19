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
                                , callApi'
                                , tj
                                , AddrAddress(..)
                                ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           Data.Vector ( Vector )
import qualified Data.Vector          as V
import           Network.Bitcoin.Types
import           Network.Browser
import           Network.HTTP hiding ( password )
import           Network.URI ( parseURI )
import qualified Data.ByteString.Lazy as BL
import           Data.Text ( Text )
import qualified Data.Text            as T

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

-- | The "no conversion needed" implementation of callApi. THis lets us inline
--   and specialize callApi for its parameters, while keeping the bulk of the
--   work in this function shared.
callApi' :: Auth -> BL.ByteString -> IO BL.ByteString
callApi' auth rpcReqBody = do
    (_, httpRes) <- browse $ do
        setOutHandler . const $ return ()
        addAuthority authority
        setAllowBasicAuth True
        request $ httpRequest (T.unpack urlString) rpcReqBody
    return $ rspBody httpRes
    where
        authority = httpAuthority auth
        urlString = rpcUrl auth

-- | 'callApi' is a low-level interface for making authenticated API
--   calls to a Bitcoin daemon. The first argument specifies
--   authentication details (URL, username, password) and is often
--   curried for convenience:
--
--   > callBtc = callApi $ Auth "http://127.0.0.1:8332" "user" "password"
--
--   The second argument is the command name.  The third argument provides
--   parameters for the API call.
--
--   > let result = callBtc "getbalance" [ tj "account-name", tj 6 ]
--
--   On error, throws a 'BitcoinException'.
callApi :: FromJSON v
        => Auth    -- ^ authentication credentials for bitcoind
        -> Text    -- ^ command name
        -> [Value] -- ^ command arguments
        -> IO v
callApi auth cmd params = readVal =<< callApi' auth jsonRpcReqBody
    where
        readVal bs = case decode' bs of
                        Just r@(BitcoinRpcResponse {btcError=NoError})
                            -> return $ btcResult r
                        Just (BitcoinRpcResponse {btcError=BitcoinRpcError code msg})
                            -> throw $ BitcoinApiError code msg
                        Nothing
                            -> throw $ BitcoinResultTypeError bs
        jsonRpcReqBody =
            encode $ object [ "jsonrpc" .= ("2.0" :: Text)
                            , "method"  .= cmd
                            , "params"  .= params
                            , "id"      .= (1 :: Int)
                            ]
{-# INLINE callApi #-}

-- | Internal helper functions to make callApi more readable
httpAuthority :: Auth -> Authority
httpAuthority (Auth urlString username password) =
    AuthBasic { auRealm    = "jsonrpc"
              , auUsername = T.unpack username
              , auPassword = T.unpack password
              , auSite     = uri
              }
    where
        uri = fromJust . parseURI $ T.unpack urlString

-- | Builds the JSON HTTP request.
httpRequest :: String -> BL.ByteString -> Request BL.ByteString
httpRequest urlString jsonBody =
    (postRequest urlString){
        rqBody = jsonBody,
        rqHeaders = [
            mkHeader HdrContentType "application/json",
            mkHeader HdrContentLength (show $ BL.length jsonBody)
        ]
    }

-- | A handy shortcut for toJSON, because I'm lazy.
tj :: ToJSON a => a -> Value
tj = toJSON
{-# INLINE tj #-}

-- | A wrapper for a vector of address:amount pairs. The RPC expects that as
--   an object of "address":"amount" pairs, instead of a vector. So that's what
--   we give them with AddrAddress's ToJSON.
newtype AddrAddress = AA (Vector (Address, BTC))

instance ToJSON AddrAddress where
    toJSON (AA vec) = object . V.toList $ uncurry (.=) <$> vec

