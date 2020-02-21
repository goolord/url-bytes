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
    Url(..)
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
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as P (skipUntil, char2, decWord16)
import qualified Data.Bytes.Parser.Unsafe as PU

-- | Url type represented by its serialization,
-- and slices of that serialization.
{- | Syntax in pseudo-BNF:

@
url = scheme ":" [ hierarchical | non-hierarchical ] [ "?" query ]? [ "#" fragment ]?
non-hierarchical = non-hierarchical-path
non-hierarchical-path = /* Does not start with "/" */
hierarchical = authority? hierarchical-path
authority = "//" userinfo? host [ ":" port ]?
userinfo = username [ ":" password ]? "@"
hierarchical-path = [ "/" path-segment ]+
@
-}
data Url = Url
  { urlSerialization :: !Bytes
  -- Components
  , urlSchemeEnd     :: !Word32 -- ^ Before @\':'@
  , urlUsernameEnd   :: !Word32 -- ^ Before @\':'@ (if a password is given) or @\'\@'@ (if not)
  , urlHostStart     :: !Word32
  , urlHostEnd       :: !Word32
  , urlPort          :: !(Maybe Word16)
  , urlPathStart     :: !Word32 -- ^ Before initial @\'/'@, if any
  , urlQueryStart    :: !Word32 -- ^ Before @\'?'@
  , urlFragmentStart :: !Word32 -- ^ Before @\'#'@
  } deriving (Eq, Ord, Show)

-- | Slice into the 'Url' and retrieve the scheme, if it's present
getScheme :: Url -> Maybe Bytes
getScheme Url{urlSerialization,urlSchemeEnd} = 
  if urlSchemeEnd == 0
    then Nothing
    else Just $ Bytes.unsafeTake (fromIntegral urlSchemeEnd) urlSerialization

-- | Slice into the 'Url' and retrieve the username, if it's present
getUsername :: Url -> Maybe Bytes
getUsername Url{urlSerialization,urlSchemeEnd,urlUsernameEnd,urlHostStart} =
  if urlUsernameEnd == urlHostStart
    then Nothing
    else Just $ unsafeSlice (urlSchemeEnd + 3) urlUsernameEnd urlSerialization

-- | Slice into the 'Url' and retrieve the host, if it's present
getHost :: Url -> Maybe Bytes
getHost Url{urlSerialization,urlHostStart,urlHostEnd} =
  if urlHostStart == urlHostEnd
    then Nothing
    else Just $ unsafeSlice urlHostStart urlHostEnd urlSerialization

-- | Slice into the 'Url' and retrieve the path starting with @\'/'@, if it's present
getPath :: Url -> Maybe Bytes
getPath Url{urlSerialization,urlPathStart,urlQueryStart} = 
  if fromIntegral urlPathStart == len
    then Nothing
    else Just $ unsafeSlice urlPathStart urlQueryStart urlSerialization
  where
  len = Bytes.length urlSerialization

-- | Slice into the 'Url' and retrieve the query string starting with @\'?'@, if it's present
getQuery :: Url -> Maybe Bytes
getQuery Url{urlSerialization,urlQueryStart,urlFragmentStart} =
  if len == fromIntegral urlQueryStart
    then Nothing
    else Just $ unsafeSlice urlQueryStart urlFragmentStart urlSerialization
  where
  len = Bytes.length urlSerialization

-- | Slice into the 'Url' and retrieve the fragment starting with @\'#'@, if it's present
getFragment :: Url -> Maybe Bytes
getFragment Url{urlSerialization,urlFragmentStart} =
  if len == fromIntegral urlFragmentStart
    then Nothing
    else Just $ unsafeSlice urlFragmentStart (fromIntegral len) urlSerialization
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

-- | Possible parse errors
data ParseError
  = EndOfInput
  | InvalidAuthority
  | InvalidPort
  deriving (Eq, Ord, Show)

-- | Decode a hierarchical URL
decodeUrl :: Bytes -> Either ParseError Url
decodeUrl urlSerialization = P.parseBytesEither (parserUrl urlSerialization) urlSerialization

-- | Parser type from @bytesmith@
-- Note: non-hierarchical Urls (such as relative paths) will not currently parse.
parserUrl :: Bytes -> P.Parser ParseError s Url
parserUrl urlSerialization = do
  -- TODO: use skipTrailedByEither later
  (!i1, !colonFirst) <- P.measure $ 
               P.skipTrailedByEither EndOfInput (c2w ':') (c2w '/')
    `P.orElse` pure False
  !urlSchemeEnd <-
    if colonFirst
      then do
        P.char2 InvalidAuthority '/' '/'
        pure $ fromIntegral $! i1 - 1
      else do
        PU.jump 0
        pure 0
  !userStart <- PU.cursor
  (!i3, _) <- P.measure $ P.skipUntil ':'
  PU.unconsume i3
  (!i4, _) <- P.measure $ P.skipUntil '@'
  PU.unconsume i4
  (!i5, _) <- P.measure $ P.skipUntil '/'
  PU.unconsume i5
  !urlUsernameEnd <- fromIntegral <$!> do
    if i4 >= i5 || i3 == i4
      then do
        PU.jump (userStart)
        pure (userStart)
      else do
        let jumpi = i4 
        if i3 < i4
          then do
            PU.jump (userStart + jumpi + 1)
            pure (userStart + i3)
          else do
            PU.jump (userStart + jumpi + 1)
            pure (userStart + i4)
  !urlHostStart' <- PU.cursor
  let !urlHostStart = fromIntegral urlHostStart'
  (!i6, !colonFirst2) <- P.measure $ 
               P.skipTrailedByEither EndOfInput (c2w ':') (c2w '/')
    `P.orElse` pure False
  (!urlHostEnd, !urlPort) <- 
    if colonFirst2
      then do
        urlHostEnd' <- fromIntegral <$!> PU.cursor
        murlPort' <- Just <$!> P.decWord16 InvalidPort
        pure (urlHostEnd' - 1, murlPort')
      else do
        PU.jump (urlHostStart' + i6 - 1)
        urlHostEnd' <- fromIntegral <$!> PU.cursor
        pure (urlHostEnd', Nothing)
  !urlPathStart <- fromIntegral <$!> PU.cursor
  (!i8, _) <- P.measure $ P.skipUntil '?'
  PU.unconsume i8
  (!i9, _) <- P.measure $ P.skipUntil '#'
  PU.unconsume i9
  let !len = fromIntegral $ Bytes.length urlSerialization
  WordPair !urlQueryStart !urlFragmentStart <- case compare i8 i9 of
    EQ -> pure $! WordPair len len
    LT -> do
      P.skipUntil '#'
      !eoi <- P.isEndOfInput
      let !urlFragmentStart' = 
            if eoi
              then len
              else fromIntegral i9 + urlPathStart
      pure $! WordPair (fromIntegral i8 + urlPathStart) urlFragmentStart'
    GT -> do
      P.skipUntil '#'
      !eoi <- P.isEndOfInput
      let !urlFragmentStart' = 
            if eoi
              then len
              else fromIntegral i9 + urlPathStart
      pure $! WordPair urlFragmentStart' urlFragmentStart'
  pure $ Url {..}

data WordPair = WordPair 
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32

{-# INLINE unsafeSlice #-}
unsafeSlice :: Word32 -> Word32 -> Bytes -> Bytes
unsafeSlice begin end (Bytes arr off _) = 
  Bytes arr (off + fromIntegral begin) (fromIntegral $ end - begin)

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}
