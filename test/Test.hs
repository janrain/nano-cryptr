{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Char8 as Char8
import System.Gnu.CryptR
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, defaultMain)
import Test.HUnit (assertEqual)

tests :: [Test]
tests =
    [ testCase "cryptR rejects empty salt" $ do
        let expected = Nothing
        let actual = cryptR "REDACTED" ""
        assertEqual "did not reject" expected actual
    , testCase "cryptR rejects salt too short" $ do
        let expected = Nothing
        let actual = cryptR "REDACTED" "$"
        assertEqual "did not reject" expected actual
    , testCase "cryptR with sha512" $ do
        let expected = Just "$6$4VMgp/9O$3lrcc/Ymn/29fXyQ.0Cv05H27Ojrxau/bZTacRHLiocN8vEtWcQWrE4skj5VgDkUJ1WS1MtR6FJp7avgQHdIf."
        let actual = cryptR "asdf3" "$6$4VMgp/9O$"
        assertEqual "incorrect password" expected actual
    ]

main :: IO ()
main = defaultMain tests
