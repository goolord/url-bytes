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
  , TemplateHaskell
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
  , getAuthority
  , getPassword
  , getHost
  , getPath
  , getQuery
  , getFragment
  , getExtension
  , getPort
  , constructUrl
  , literalUrl
  ) where

import Data.Word (Word16)
import Data.Bytes.Types (Bytes(..))
import Url.Rebind (decodeUrl)
import Url.Unsafe (Url(..),ParseError(..))
import GHC.Exts (Int(I#),(==#),Int#,int2Word#)
import GHC.Word (Word16(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (TExp(TExp))
import Data.List (intercalate)
import GHC.Integer.GMP.Internals (Integer(..))
import qualified Data.Bytes as Bytes

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

getAuthority :: Url -> Maybe Bytes
getAuthority Url{urlSerialization,urlSchemeEnd,urlUsernameEnd,urlHostStart} =
  case urlUsernameEnd ==# urlHostStart of
    0# -> Just $ unsafeSlice (I# urlSchemeEnd + 3) (I# urlHostStart - 1) urlSerialization
    _ -> Nothing

getPassword :: Url -> Maybe Bytes
getPassword Url{urlSerialization,urlUsernameEnd,urlHostStart} =
  case urlUsernameEnd ==# urlHostStart of
    0# -> 
      let mpass = unsafeSlice (I# urlUsernameEnd) (I# urlHostStart - 1) urlSerialization
      in case Bytes.uncons mpass of
        Just (58,password) -> Just password
        _ -> Nothing
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

getPort :: Url -> Maybe Word16
getPort Url{urlPort} =
  case urlPort of
    0x10000# -> Nothing
    x -> Just $ W16# (int2Word# x)

-- | This function is intentionally imprecise. 
-- E.g. @getExtension "google.com/facebook.com" == Just ".com"@
getExtension :: Url -> Maybe Bytes
getExtension url = do
  path <- getPath url
  if not (Bytes.elem 0x2e path)
  then Nothing
  else case Bytes.split 0x2e path of
    [] -> Nothing
    xs -> Just $ last xs

{-# INLINE unsafeSlice #-}
unsafeSlice :: Int -> Int -> Bytes -> Bytes
unsafeSlice begin end (Bytes arr _ _) = 
  Bytes arr begin (end - begin)

literalUrl :: String -> Q (TExp Url)
literalUrl ser = case decodeUrl $ Bytes.fromLatinString ser of
  Left e -> fail $ "Invalid url. Parse error: " <> show e
  Right Url{..} -> do
    pure $ TExp $
      ConE 'Url
        `AppE` (ParensE $ (VarE 'Bytes.fromLatinString) `AppE` (LitE $ StringL ser))
        `AppE` (liftInt# urlSchemeEnd)
        `AppE` (liftInt# urlUsernameEnd)
        `AppE` (liftInt# urlHostStart)
        `AppE` (liftInt# urlHostEnd)
        `AppE` (liftInt# urlPort)
        `AppE` (liftInt# urlPathStart)
        `AppE` (liftInt# urlQueryStart)
        `AppE` (liftInt# urlFragmentStart)
  where
  liftInt# :: Int# -> Exp
  liftInt# x = LitE (IntPrimL (S# x))

constructUrl ::
     Maybe String -- ^ scheme
  -> String -- ^ host
  -> Maybe Word16 -- ^ port
  -> String -- ^ path
  -> [(String,String)] -- query string params
  -> Maybe String -- ^ framgent
  -> Q (TExp Url)
constructUrl mscheme host mport path qps mfrag = literalUrl ser
  where
  ser = scheme <> host <> port <> path <> rqps <> frag
  scheme = case mscheme of
    Nothing -> mempty
    Just x -> x <> "://"
  port = case mport of
    Nothing -> mempty
    Just x -> ':' : show x
  rqps :: String
  rqps = "?" <> (intercalate "&" $ fmap (\(a,b) -> a <> "=" <> b) qps)
  frag = case mfrag of
    Nothing -> mempty
    Just x -> "#" <> x
