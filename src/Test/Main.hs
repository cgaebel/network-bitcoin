{-# LANGUAGE OverloadedStrings #-}

module Main where


import Test.QuickCheck
import Test.QuickCheck.Monadic
import Network.Bitcoin
import Data.Aeson
import Data.Vector ( empty, mapM )


main :: IO ()
main = mapM_ qcOnce [ canGetInfo
                    , canListUnspent
                    , canGetOutputInfo
                    ]


qcOnce :: Property -> IO ()
qcOnce = quickCheckWith stdArgs { maxSuccess = 1
                                , maxSize = 1
                                , maxDiscardRatio = 1
                                }


auth :: Auth
auth = Auth "http://localhost:18332" "bitcoinrpc" "bitcoinrpcpassword"


canGetInfo :: Property
canGetInfo = monadicIO $ do
    info <- run $ getBitcoindInfo auth
    let checks = [ bitcoinVersion info > 80000
                 , onTestNetwork info
                 , bitcoindErrors info == ""
                 ]
    assert $ and checks


canListUnspent :: Property
canListUnspent = monadicIO $ do
    _ <- run $ listUnspent auth Nothing Nothing Data.Vector.empty
    assert True


canGetOutputInfo :: Property
canGetOutputInfo = monadicIO $ do
    info <- run $ getOutputInfo auth "ab8e26fd95fa371ac15b43684d0c6797fb573757095e7d763ba86ad315f7db04" 1
    _ <- run $ print info
    assert True
