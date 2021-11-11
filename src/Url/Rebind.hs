{-# LANGUAGE
    BangPatterns
  , UnboxedTuples
  , UnboxedSums
  , MagicHash
  , RebindableSyntax
  , ScopedTypeVariables
  , RecordWildCards
  , NamedFieldPuns
#-}

-- The parser lives in its own module because it uses RebindableSyntax,
-- which adversely affects inference and error messages.
module Url.Rebind
  ( decodeUrl
  ) where

import Prelude hiding ((>>=),(>>),pure)
import Data.Bytes.Parser.Rebindable ((>>),(>>=),pure)

import Data.Bytes.Types (Bytes(..))
import Data.Char (ord)
import Data.Word (Word8)
import GHC.Exts (Int(I#),Int#,(+#),(<#),(-#),orI#,(>=#),(==#),(>#))
import GHC.Word (Word16(W16#))
import Url.Unsafe (Url(..),ParseError(..))
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as P (skipUntil, char, char2, decWord16, skipDigits1)
import qualified Data.Bytes.Parser.Unsafe as PU
import qualified GHC.Exts as Exts

-- | Decode a hierarchical URL
decodeUrl :: Bytes -> Either ParseError Url
decodeUrl urlSerialization = P.parseBytesEither parserUrl urlSerialization

parserAuthority :: Int# -> P.Parser ParseError s (# Int#, Int#, Int#, Int#, Int# #)
{-# inline parserAuthority #-}
parserAuthority urlSchemeEnd = do
  userStart <- PU.cursor#
  i3 <- P.measure_# (P.skipUntil ':')
  PU.unconsume (I# i3)
  i4 <- P.measure_# (P.skipUntil '@')
  PU.unconsume (I# i4)
  i5 <- P.measure_# (P.skipUntil '/')
  PU.unconsume (I# i5)
  urlUsernameEnd <- ( case (i4 >=# i5) `orI#` (i3 ==# i4) of
          1# -> PU.jump (I# userStart) >> pure userStart
          _ -> let jumpi = i4 in
            case i3 <# i4 of
              1# -> PU.jump (I# (userStart +# jumpi +# 1# )) >> pure (userStart +# i3)
              _ -> PU.jump (I# (userStart +# jumpi +# 1# )) >> pure (userStart +# i4)
      )
  urlHostStart <- PU.cursor#
  colonSlashNeither <- P.skipTrailedBy2# EndOfInput (c2w ':') (c2w '/') `orElse#` 2#
  (# !urlHostEnd, !urlPort #) <- case colonSlashNeither of
    0# -> do
      urlHostEnd <- PU.cursor# -- ':' encountered first
      (W16# urlPort) <- P.decWord16 InvalidPort
      pure (# urlHostEnd -# 1#, Exts.word2Int# urlPort #)
    1# -> do -- '/' encountered first
      urlHostEnd' <- PU.cursor#
      -- Backing up by one since we want to put the slash back
      -- to let it be part of the path.
      let urlHostEnd = urlHostEnd' -# 1#
      PU.jump (I# urlHostEnd)
      pure (# urlHostEnd, 0x10000# #)
    _ -> do -- neither encountered
      urlHostEnd <- PU.cursor#
      PU.jump (I# urlHostEnd)
      pure (# urlHostEnd, 0x10000# #)
  pure
    (# urlSchemeEnd 
    , urlUsernameEnd 
    , urlHostStart  
    , urlHostEnd    
    , urlPort       
    #)

-- | Parser type from @bytesmith@
-- Note: non-hierarchical Urls (such as relative paths) will not currently parse.
parserUrl :: P.Parser ParseError s Url
parserUrl = do
  urlSerialization@(Bytes _ _ (I# len)) <- P.peekRemaining 
  start <- PU.cursor#
  (I# i1, !slashFirst) <- P.measure $
               P.skipTrailedBy2 EndOfInput (c2w ':') (c2w '/')
    `P.orElse` pure True
  (# urlSchemeEnd, urlUsernameEnd, urlHostStart, urlHostEnd, urlPort #) <- case slashFirst of
    False ->
      -- If we see something like "abc://" with a colon followed by two
      -- slashes, we assume that the authority is present ("abc" in
      -- this case).
      succeeded (P.char2 InvalidAuthority '/' '/') >>= \hasAuthorityA -> do
        let urlSchemeEnd = (i1 -# 1# )
        case hasAuthorityA of
          0# -> succeeded (P.skipDigits1 () >> P.char () '/') >>= \hasAuthorityB ->
            -- Here, we are looking for things like "example.com:8888/" that
            -- are missing the scheme but include a port.
            case hasAuthorityB of
              0# -> pure (# urlSchemeEnd, urlSchemeEnd, urlSchemeEnd, urlSchemeEnd, 0x10000# #)
              _ -> PU.jump (I# start) >> parserAuthority start
          _ -> parserAuthority urlSchemeEnd
    True ->
      PU.jump (I# start) >> parserAuthority start 
  urlPathStart <- PU.cursor#
  i8 <- P.measure_# (P.skipUntil '?')
  PU.unconsume (I# i8)
  i9 <- P.measure_# (P.skipUntil '#')
  PU.unconsume (I# i9)
  (# !urlQueryStart, !urlFragmentStart #) <- case intCompare# i8 i9 of
    EQ -> pure (# len, len #)
    LT -> P.skipUntil '#' >> 
      let !urlFragmentStart = i9 +# urlPathStart
       in pure (# (i8 +# urlPathStart), urlFragmentStart #)
    GT -> P.skipUntil '#' >> 
      let !urlFragmentStart = i9 +# urlPathStart
       in pure (# urlFragmentStart, urlFragmentStart #)
  pure (Url {..})

intCompare# :: Int# -> Int# -> Ordering
intCompare# a b = case a ==# b of
  0# -> case a ># b of
    0# -> LT
    _ -> GT
  _ -> EQ

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

orElse# :: forall e x s. PU.Parser x s Int# -> Int# -> PU.Parser e s Int#
{-# inline orElse# #-}
orElse# (PU.Parser f) i = PU.Parser
  (\x@(# _, b, c #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# _ | #) -> (# s1, (# | (# i, b, c #) #) #)
      (# | r #) -> (# s1, (# | r #) #)
  )

-- Runs the parser, returning 1 if it succeeds and 0 if it fails.
-- Rolls back on failure, but consumes on success.
succeeded :: PU.Parser x s a -> PU.Parser e s Int#
{-# inline succeeded #-}
succeeded (PU.Parser f) = PU.Parser
  (\x@(# _, b, c #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# _ | #) -> (# s1, (# | (# 0#, b, c #) #) #)
      (# | (# _, b1, c1 #) #) -> (# s1, (# | (# 1#, b1, c1 #) #) #)
  )

