{-# LANGUAGE
    OverloadedStrings
  , BangPatterns
  , UnboxedTuples
  , UnboxedSums
  , MagicHash
  , ScopedTypeVariables
  , LambdaCase
  , RecordWildCards
  , NamedFieldPuns
  , ApplicativeDo
#-}

-- | Note: this library parses, but does not validate urls
module Url 
  ( -- * Types
    Url(urlSerialization)
  , ParseError(..)
    -- * Parsing
  , decodeUrl
    -- * Slicing
  , getScheme
  , getUsername
  , getHost
  , getPath
  , getQuery
  , getFragment
  , getExtension
  ) where

import Data.Word (Word16)
import Data.Bytes.Types (Bytes(..))
import Url.Rebind (decodeUrl)
import Url.Unsafe (Url(..),ParseError(..))
import GHC.Exts (Int(I#),(==#))
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P

-- | Slice into the 'Url' and retrieve the scheme, if it's present
getScheme :: Url -> Maybe Bytes
getScheme Url{urlSerialization,urlSchemeEnd} = 
  if I# urlSchemeEnd == 0
    then Nothing
    else Just $ Bytes.unsafeTake (I# urlSchemeEnd) urlSerialization

-- | Slice into the 'Url' and retrieve the username, if it's present
getUsername :: Url -> Maybe Bytes
getUsername Url{urlSerialization,urlSchemeEnd,urlUsernameEnd,urlHostStart} =
  case urlUsernameEnd ==# urlHostStart of
    0# -> Just $ unsafeSlice (I# urlSchemeEnd + 3) (I# urlUsernameEnd) urlSerialization
    _ -> Nothing

-- | Slice into the 'Url' and retrieve the host, if it's present
getHost :: Url -> Maybe Bytes
getHost Url{urlSerialization,urlHostStart,urlHostEnd} =
  case urlHostStart ==# urlHostEnd of
    0# -> Just $ unsafeSlice (I# urlHostStart) (I# urlHostEnd) urlSerialization
    _ -> Nothing

-- | Slice into the 'Url' and retrieve the path starting with @\'/'@, if it's present
getPath :: Url -> Maybe Bytes
getPath Url{urlSerialization,urlPathStart,urlQueryStart} = 
  case urlPathStart ==# len of
    0# -> Just $ unsafeSlice (I# urlPathStart) (I# urlQueryStart) urlSerialization
    _ -> Nothing
  where
  !(I# len) = Bytes.length urlSerialization

-- | Slice into the 'Url' and retrieve the query string starting with @\'?'@, if it's present
getQuery :: Url -> Maybe Bytes
getQuery Url{urlSerialization,urlQueryStart,urlFragmentStart} =
  case len ==# urlQueryStart of
    0# -> Just $ unsafeSlice (I# urlQueryStart) (I# urlFragmentStart) urlSerialization
    _ -> Nothing
  where
  !(I# len) = Bytes.length urlSerialization

-- | Slice into the 'Url' and retrieve the fragment starting with @\'#'@, if it's present
getFragment :: Url -> Maybe Bytes
getFragment Url{urlSerialization,urlFragmentStart} =
  case len ==# urlFragmentStart of
    0# -> Just $ unsafeSlice (I# urlFragmentStart) (I# len) urlSerialization
    _ -> Nothing
  where
  !(I# len) = Bytes.length urlSerialization

-- | This function is intentionally imprecise. 
-- E.g. @getExtension "google.com/facebook.com" == Just ".com"@
getExtension :: Url -> Maybe Bytes
getExtension url = do
  path <- getPath url
  P.parseBytesMaybe parserExtension path

parserExtension :: P.Parser () s Bytes
parserExtension = do
  until_ not $ succeeded $ P.skipTrailedBy () 47
  until_ not $ succeeded $ P.skipTrailedBy () 46
  rest <- P.remaining
  if Bytes.null rest
    then P.fail ()
    else pure rest

succeeded :: P.Parser e s a -> P.Parser e s Bool
succeeded p = (True <$ p) `P.orElse` pure False

until_ :: (Monad m) => (a -> Bool) -> m a -> m ()
until_ p m = go
  where
  go = do 
    x <- m
    if p x
      then return ()
      else go

{-# INLINE unsafeSlice #-}
unsafeSlice :: Int -> Int -> Bytes -> Bytes
unsafeSlice begin end (Bytes arr _ _) = 
  Bytes arr begin (end - begin)
