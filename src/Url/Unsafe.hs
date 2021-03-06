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

{-# OPTIONS_HADDOCK not-home #-}

module Url.Unsafe
  ( -- * Types
    Url(..)
  , ParseError(..)
  ) where

import Data.Bytes.Types (Bytes(..))
import GHC.Exts (Int#)

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
  { urlSerialization :: {-# UNPACK #-} !Bytes
  -- Components
  , urlSchemeEnd     :: !Int# -- ^ Before @\':'@
  , urlUsernameEnd   :: !Int# -- ^ Before @\':'@ (if a password is given) or @\'\@'@ (if not)
  , urlHostStart     :: !Int#
  , urlHostEnd       :: !Int#
  , urlPort          :: !Int#
  , urlPathStart     :: !Int# -- ^ Before initial @\'/'@, if any
  , urlQueryStart    :: !Int# -- ^ Before @\'?'@
  , urlFragmentStart :: !Int# -- ^ Before @\'#'@
  } deriving (Eq, Ord, Show)

-- | Possible parse errors
data ParseError
  = EndOfInput
  | InvalidAuthority
  | InvalidPort
  deriving (Eq, Ord, Show)
