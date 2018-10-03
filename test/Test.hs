{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as Char8
import System.Gnu.CryptR
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test, defaultMain)
import Test.HUnit (assertEqual)


tests :: [Test]
tests =
    [ testCase "cryptR rejects empty salt" $ do
        let expected = Nothing
        let actual = cryptR "REDACTED" ""
        assertEqual "did not reject" expected actual
    , testCase "cryptR rejects invalid salt" $ do
        let expected = Nothing
        let actual = cryptR "REDACTED" "$"
        assertEqual "did not reject" expected actual
    , testCase "cryptR accepts empty password" $ do
        let expected = Just "$6$4VMgp/9O$z6upTH9iBjZLQufNBBtJDaY15jAzHcuHc215/zk2ki5zss1AbxUQB7ldx3646vgO628eBk8tViZQUCRYASYtw."
        let actual = cryptR "" "$6$4VMgp/9O$"
        assertEqual "did not accept" expected actual
    , testCase "cryptR with DES" $ do
        let expected = Just "SAOU76h2fjH0k"
        let actual = cryptR "REDACTED" "SALT"
        assertEqual "incorrect hash" expected actual
    , testCase "cryptR with MD5" $ do
        let expected = Just "$1$etNnh7FA$Ki37w5fDrNyod/DTZKNMI0"
        let actual = cryptR "REDACTED" "$1$etNnh7FA$"
        assertEqual "incorrect hash" expected actual
    , testCase "cryptR with SHA-256" $ do
        let expected = Just "$5$9ks3nNEqv31FX.F$woms7JW5OMylOK5D9mCO1ZWtVPYd9fefC9tAC98vej1"
        let actual = cryptR "REDACTED" "$5$9ks3nNEqv31FX.F$"
        assertEqual "incorrect hash" expected actual
    , testCase "cryptR with SHA-512" $ do
        let expected = Just "$6$4VMgp/9O$O3uYU1gtPUrJkTIDwWtylzztmaHiwUO/KsK9d6QpAvMUOVbSeYy5DY4lxO6YZJoakJhwAgB2H406paso6KPpR/"
        let actual = cryptR "REDACTED" "$6$4VMgp/9O$"
        assertEqual "incorrect hash" expected actual

    -- These tests are here to verify that cryptR won't crash on invalid inputs,
    -- not to assert interesting properties of the crypt function.
    , testProperty "cryptR does not crash on arbitrary passwords" propArbitraryPassword
    , testProperty "cryptR does not crash on arbitrary salts" propArbitrarySalt
    ]
  where
    propArbitraryPassword bytes = do
      let pass = Bytes.pack bytes
      let salt = "$6$4VMgp/9O$"
      case cryptR pass salt of
        Nothing -> False -- Given a well-formed salt, this should not happen
        Just hash -> do
          and [ salt == Char8.take (Char8.length salt) hash
              , Char8.length hash > Char8.length salt
              ]

    propArbitrarySalt bytes = do
      let pass = "REDACTED"
      let salt = Bytes.pack bytes
      case cryptR pass salt of
        Nothing -> True -- Rejecting invalid salts is OK
        Just hash -> not (Char8.null hash)


main :: IO ()
main = defaultMain tests
