module Main where

import Data.Bytes
import Test.Tasty
import Test.Tasty.HUnit
import Url

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "URL decoding test 1" $
      decodeUrl urlBytes1 @?= Right url1
  , testCase "URL decoding test 2" $
      decodeUrl urlBytes2 @?= Right url2
  , testCase "URL decoding test 3" $
      decodeUrl urlBytes3 @?= Right url3
  , testCase "getScheme" $
      getScheme url2 @?= Just (fromAsciiString "http")
  , testCase "getUsername" $
      getUsername url2 @?= Just (fromAsciiString "user")
  , testCase "getHost" $
      getHost url2 @?= Just (fromAsciiString "facebook.org")
  , testCase "getPath" $
      getPath url1 @?= Just (fromAsciiString "/foo")
  , testCase "getQuery" $
      getQuery url1 @?= Just (fromAsciiString "?bar=qux")
  , testCase "getFragment" $
      getFragment url1 @?= Just (fromAsciiString "#quine")
  ]

urlBytes1 :: Bytes
urlBytes1 = fromAsciiString "https://google.com/foo?bar=qux#quine"

url1 :: Url
url1 = Url
  { urlSerialization = urlBytes1
  , urlSchemeEnd     = 5
  , urlUsernameEnd   = 8
  , urlHostStart     = 8
  , urlHostEnd       = 18
  , urlPort          = Nothing
  , urlPathStart     = 18
  , urlQueryStart    = Just 22
  , urlFragmentStart = Just 30
  }

urlBytes2 :: Bytes
urlBytes2 = fromAsciiString "http://user:password@facebook.org:322/"

url2 :: Url
url2 = Url
  { urlSerialization = urlBytes2
  , urlSchemeEnd     = 4
  , urlUsernameEnd   = 11
  , urlHostStart     = 21
  , urlHostEnd       = 33
  , urlPort          = Just 322
  , urlPathStart     = 37
  , urlQueryStart    = Nothing
  , urlFragmentStart = Nothing
  }

urlBytes3 :: Bytes
urlBytes3 = fromAsciiString "x@g/f/:/@?#"

url3 :: Url
url3 = Url
  { urlSerialization = urlBytes3
  , urlSchemeEnd = 0
  , urlUsernameEnd = 1
  , urlHostStart = 2
  , urlHostEnd = 3
  , urlPort = Nothing
  , urlPathStart = 3
  , urlQueryStart = Just 9
  , urlFragmentStart = Just 10
  }
