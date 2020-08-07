{-# LANGUAGE 
    MagicHash 
  , TemplateHaskell
#-}

module Main where

import Control.Monad
import Data.Bytes
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
import Url
import Url.Unsafe

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
  , testCase "URL decoding test 4" $
      when (isLeft (decodeUrl urlBytes4)) (assertFailure "")
  , testCase "getScheme" $
      getScheme url2 @?= Just (fromAsciiString "http")
  , testCase "getUsername" $
      getUsername url2 @?= Just (fromAsciiString "user")
  , testCase "getPassword" $
      getPassword url2 @?= Just (fromAsciiString "password")
  , testCase "getAuthority" $
      getAuthority url2 @?= Just (fromAsciiString "user:password")
  , testCase "getHost" $
      getHost url2 @?= Just (fromAsciiString "facebook.org")
  , testCase "getPath" $
      getPath url1 @?= Just (fromAsciiString "/foo")
  , testCase "getQuery" $
      getQuery url1 @?= Just (fromAsciiString "?bar=qux")
  , testCase "getQuery 2" $
      getQuery url3 @?= Just (fromAsciiString "?")
  , testCase "getQuery 3" $
      getQuery url2 @?= Nothing
  , testCase "getFragment" $
      getFragment url1 @?= Just (fromAsciiString "#quine")
  , testCase "getFragment 2" $
      getFragment url3 @?= Just (fromAsciiString "#")
  , testCase "getFragment 3" $
      getFragment url2 @?= Nothing
  , testCase "getExtension" $ do
      let eurl1 = getExtension $ unwrap $ decodeUrl $ fromAsciiString "https://imgur.com/./f.o.o.png?bar=bar#q"
          eurl2 = getExtension $ unwrap $ decodeUrl $ fromAsciiString "foo.com/sustainability/bikeuab/commutesmart/itemlist/user/79-2020-05-20-18-13-31&format=feed&Itemid=597&format=feed&Itemid=597&type=rss&format=feed&Itemid=597&type=rss&format=feed&Itemid=597&type=rss&format=feed&Itemid=597"
          eurl3 = getExtension $ unwrap $ decodeUrl $ fromAsciiString "foo.com/news/health/item/11418-o-neal-cancer-center-at-uab-urges-continued-cancer-screenings-during-pandemic"
      eurl1 @?= Just (fromAsciiString "png")
      eurl2 @?= Nothing
      eurl3 @?= Nothing
  , testCase "offsetUrl" $ do
      let urlBytes1Offset = unsafeDrop 2 $ fromAsciiString "  " <> urlBytes1
          Right url1Offset = decodeUrl urlBytes1Offset
      getPath url1 @?= getPath url1Offset
  , testCase "constructUrl" $ do
      url1TH1 @?= url1
      url1TH2 @?= url1
  ]

unwrap :: Either a b -> b
unwrap = either (error "unwrap") id

urlBytes1 :: Bytes
urlBytes1 = fromAsciiString "https://google.com/foo?bar=qux#quine"

url1TH1 :: Url
url1TH1 = $$(constructUrl (Just "https") "google.com" Nothing "/foo" [("bar", "qux")] (Just "quine"))

url1TH2 :: Url
url1TH2 = $$(literalUrl "https://google.com/foo?bar=qux#quine")

url1 :: Url
url1 = Url
  { urlSerialization = urlBytes1
  , urlSchemeEnd     = 5#
  , urlUsernameEnd   = 8#
  , urlHostStart     = 8#
  , urlHostEnd       = 18#
  , urlPort          = 0x10000#
  , urlPathStart     = 18#
  , urlQueryStart    = 22#
  , urlFragmentStart = 30#
  }

urlBytes2 :: Bytes
urlBytes2 = fromAsciiString "http://user:password@facebook.org:322/"

url2 :: Url
url2 = Url
  { urlSerialization = urlBytes2
  , urlSchemeEnd     = 4#
  , urlUsernameEnd   = 11#
  , urlHostStart     = 21#
  , urlHostEnd       = 33#
  , urlPort          = 322#
  , urlPathStart     = 37#
  , urlQueryStart    = 38#
  , urlFragmentStart = 38#
  }

urlBytes3 :: Bytes
urlBytes3 = fromAsciiString "x@g/f/:/@?#"

url3 :: Url
url3 = Url
  { urlSerialization = urlBytes3
  , urlSchemeEnd     = 0#
  , urlUsernameEnd   = 1#
  , urlHostStart     = 2#
  , urlHostEnd       = 3#
  , urlPort          = 0x10000#
  , urlPathStart     = 3#
  , urlQueryStart    = 9#
  , urlFragmentStart = 10#
  }

urlBytes4 :: Bytes
urlBytes4 = fromAsciiString "file+udp:bar.txt"

