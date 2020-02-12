{-# LANGUAGE
    OverloadedStrings
  , BangPatterns
  , UnboxedTuples
  , UnboxedSums
  , MagicHash
  , ScopedTypeVariables
  , LambdaCase
  , RecordWildCards
#-}

-- | Note: this library parses, but does not validate urls
module Url 
  ( Url(..)
  , ParseError(..)
  , decodeUrl
  , parserUrl
  ) where

import Data.Word
import Data.Bytes
import Control.Monad (when)
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as P
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
  , urlSchemeEnd     :: !Word32 -- Before ':: '
  , urlUsernameEnd   :: !Word32 -- Before ':: ' (if a password is given) or '@' (if not)
  , urlHostStart     :: !Word32
  , urlHostEnd       :: !Word32
  , urlPort          :: !(Maybe Word16)
  , urlPathStart     :: !Word32         -- Before initial '/', if any
  , urlQueryStart    :: !(Maybe Word32) -- Before '?', unlike Position :: ::QueryStart
  , urlFragmentStart :: !(Maybe Word32) -- Before '#', unlike Position :: ::FragmentStart
  } deriving (Eq, Ord, Show)

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
  (i1, _) <- P.measure $ P.skipUntil ':'
  PU.unconsume i1
  (i2, _) <- P.measure $ P.skipUntil '/'
  urlSchemeEnd <-
    if i1 < i2
      then do
        PU.jump i1
        pure $ fromIntegral i1
      else do
        PU.jump 0
        pure 0
  when (urlSchemeEnd /= 0) $ P.char3 InvalidAuthority ':' '/' '/'
  userStart <- PU.cursor
  (i3, _) <- P.measure $ P.skipUntil ':'
  PU.unconsume i3
  (i4, _) <- P.measure $ P.skipUntil '@'
  PU.unconsume i4
  (i5, _) <- P.measure $ P.skipUntil '/'
  PU.unconsume i5
  urlUsernameEnd <- fromIntegral <$> do
    if i4 >= i5 || i3 == i4
      then do
        PU.jump (userStart + 1)
        pure (userStart + 1)
      else do
        let jumpi = i4 
        if i3 < i4
          then do
            PU.jump (userStart + jumpi + 2)
            pure (userStart + i3)
          else do
            PU.jump (userStart + jumpi)
            pure (userStart + i4)
  urlHostStart' <- PU.cursor
  let urlHostStart = fromIntegral urlHostStart'
  (i6, _) <- P.measure $ P.skipUntil '/'
  PU.unconsume i6
  (i7, _) <- P.measure $ P.skipUntil ':'
  PU.unconsume i7
  (urlHostEnd, urlPort) <- 
    if i7 < i6
      then do
        PU.jump (urlHostStart' + i7)
        urlHostEnd' <- fromIntegral <$> PU.cursor
        P.char InvalidPort ':'
        murlPort' <- Just <$> P.decWord16 InvalidPort
        pure (urlHostEnd', murlPort')
      else do
        PU.jump (urlHostStart' + i6)
        urlHostEnd' <- fromIntegral <$> PU.cursor
        pure (urlHostEnd', Nothing)
  urlPathStart <- fromIntegral <$> PU.cursor
  (i8, _) <- P.measure $ P.skipUntil '?'
  PU.unconsume i8
  (i9, _) <- P.measure $ P.skipUntil '#'
  PU.unconsume i9
  (urlQueryStart, urlFragmentStart) <- case compare i8 i9 of
    EQ -> pure (Nothing, Nothing)
    LT -> do
      P.skipUntil '#'
      eoi <- P.isEndOfInput
      let urlFragmentStart' = 
            if eoi
              then Nothing
              else Just $ fromIntegral i9 + urlPathStart
      pure (Just $ fromIntegral i8 + urlPathStart, urlFragmentStart')
    GT -> do
      P.skipUntil '#'
      eoi <- P.isEndOfInput
      let urlFragmentStart' = 
            if eoi
              then Nothing
              else Just $ fromIntegral i9 + urlPathStart
      pure (Nothing, urlFragmentStart')
  pure $ Url {..}

