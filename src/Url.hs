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
    Url
  , ParseError(..)
    -- * Parsing
  , decodeUrl
  , parserUrl
    -- * Slicing
  , getScheme
  , getUsername
  , getHost
  , getPath
  , getQuery
  , getFragment
  , getExtension
  ) where

import Control.Monad ((<$!>))
import Data.Char (ord)
import Data.Word (Word8, Word32, Word16)
import Data.Bytes.Types (Bytes(..))
import Url.Rebind (parserUrl)
import Url.Unsafe (Url(..),ParseError(..))
import GHC.Exts (Int(I#))
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as P (skipUntil, char2, decWord16)
import qualified Data.Bytes.Parser.Unsafe as PU

-- | Slice into the 'Url' and retrieve the scheme, if it's present
getScheme :: Url -> Maybe Bytes
getScheme Url{urlSerialization,urlSchemeEnd} = 
  if I# urlSchemeEnd == 0
    then Nothing
    else Just $ Bytes.unsafeTake (I# urlSchemeEnd) urlSerialization

-- | Slice into the 'Url' and retrieve the username, if it's present
getUsername :: Url -> Maybe Bytes
getUsername Url{urlSerialization,urlSchemeEnd,urlUsernameEnd,urlHostStart} =
  if I# urlUsernameEnd == I# urlHostStart
    then Nothing
    else Just $ unsafeSlice (I# urlSchemeEnd + 3) (I# urlUsernameEnd) urlSerialization

-- | Slice into the 'Url' and retrieve the host, if it's present
getHost :: Url -> Maybe Bytes
getHost Url{urlSerialization,urlHostStart,urlHostEnd} =
  if I# urlHostStart == I# urlHostEnd
    then Nothing
    else Just $ unsafeSlice (I# urlHostStart) (I# urlHostEnd) urlSerialization

-- | Slice into the 'Url' and retrieve the path starting with @\'/'@, if it's present
getPath :: Url -> Maybe Bytes
getPath Url{urlSerialization,urlPathStart,urlQueryStart} = 
  if I# urlPathStart == len
    then Nothing
    else Just $ unsafeSlice (I# urlPathStart) (I# urlQueryStart) urlSerialization
  where
  len = Bytes.length urlSerialization

-- | Slice into the 'Url' and retrieve the query string starting with @\'?'@, if it's present
getQuery :: Url -> Maybe Bytes
getQuery Url{urlSerialization,urlQueryStart,urlFragmentStart} =
  if len == I# urlQueryStart
    then Nothing
    else Just $ unsafeSlice (I# urlQueryStart) (I# urlFragmentStart) urlSerialization
  where
  len = Bytes.length urlSerialization

-- | Slice into the 'Url' and retrieve the fragment starting with @\'#'@, if it's present
getFragment :: Url -> Maybe Bytes
getFragment Url{urlSerialization,urlFragmentStart} =
  if len == I# urlFragmentStart
    then Nothing
    else Just $ unsafeSlice (I# urlFragmentStart) len urlSerialization
  where
  len = Bytes.length urlSerialization

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

-- | Decode a hierarchical URL
decodeUrl :: Bytes -> Either ParseError Url
decodeUrl urlSerialization = P.parseBytesEither (parserUrl urlSerialization) urlSerialization

data WordPair = WordPair 
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32

{-# INLINE unsafeSlice #-}
unsafeSlice :: Int -> Int -> Bytes -> Bytes
unsafeSlice begin end (Bytes arr off _) = 
  Bytes arr (off + begin) (end - begin)

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}
