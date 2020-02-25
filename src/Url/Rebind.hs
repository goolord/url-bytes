{-# LANGUAGE
    OverloadedStrings
  , BangPatterns
  , UnboxedTuples
  , UnboxedSums
  , MagicHash
  , RebindableSyntax
  , ScopedTypeVariables
  , LambdaCase
  , RecordWildCards
  , NamedFieldPuns
  , ApplicativeDo
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
import qualified Data.Bytes.Parser.Latin as P (skipUntil, char2, decWord16)
import qualified Data.Bytes.Parser.Unsafe as PU
import qualified GHC.Exts as Exts

-- | Decode a hierarchical URL
decodeUrl :: Bytes -> Either ParseError Url
decodeUrl urlSerialization = P.parseBytesEither parserUrl urlSerialization

-- | Parser type from @bytesmith@
-- Note: non-hierarchical Urls (such as relative paths) will not currently parse.
parserUrl :: P.Parser ParseError s Url
parserUrl = 
  P.peekRemaining 
  >>= \urlSerialization@(Bytes _ _ (I# len)) ->
  PU.cursor#
  >>= \start ->
  P.measure
    ( P.skipTrailedBy2 EndOfInput (c2w ':') (c2w '/')
      `P.orElse`
      pure True
    )
  >>= \(I# i1, !slashFirst) ->
    ( case slashFirst of
        False ->
          P.char2 InvalidAuthority '/' '/' >> pure (i1 -# 1# )
        True ->
          PU.jump (I# start) >> pure start
    )
  >>= \urlSchemeEnd -> PU.cursor#
  >>= \userStart -> P.measure_# (P.skipUntil ':')
  >>= \i3 -> PU.unconsume (I# i3)
  >>  P.measure_# (P.skipUntil '@')
  >>= \i4 -> PU.unconsume (I# i4)
  >>  P.measure_# (P.skipUntil '/')
  >>= \i5 -> PU.unconsume (I# i5)
  >>  ( case (i4 >=# i5) `orI#` (i3 ==# i4) of
          1# -> PU.jump (I# userStart) >> pure userStart
          _ -> let jumpi = i4 in
            case i3 <# i4 of
              1# -> PU.jump (I# (userStart +# jumpi +# 1# )) >> pure (userStart +# i3)
              _ -> PU.jump (I# (userStart +# jumpi +# 1# )) >> pure (userStart +# i4)
      )
  >>= \urlUsernameEnd -> PU.cursor#
  >>= \urlHostStart ->
    ( P.skipTrailedBy2# EndOfInput (c2w ':') (c2w '/')
      `orElse#` 2#
    )
  >>= \colonSlashNeither ->
    ( case colonSlashNeither of
        0# -> PU.cursor# -- ':' encountered first
          >>= \urlHostEnd -> P.decWord16 InvalidPort
          >>= \(W16# urlPort) ->
          pure (# urlHostEnd -# 1#, Exts.word2Int# urlPort #)
        1# -> -- '/' encountered first
              PU.cursor#
          >>= \urlHostEnd' ->
              -- Backing up by one since we want to put the slash back
              -- to let it be part of the path.
              let urlHostEnd = urlHostEnd' -# 1# in
              PU.jump (I# urlHostEnd)
          >>  pure (# urlHostEnd, 0x10000# #)
        _ -> -- neither encountered
              PU.cursor#
          >>= \urlHostEnd ->
              PU.jump (I# urlHostEnd)
          >>  pure (# urlHostEnd, 0x10000# #)
    )
  >>= \(# !urlHostEnd, !urlPort #) -> PU.cursor#
  >>= \urlPathStart -> P.measure_# (P.skipUntil '?')
  >>= \i8 -> PU.unconsume (I# i8)
  >>  P.measure_# (P.skipUntil '#')
  >>= \i9 -> PU.unconsume (I# i9)
  >>  ( case intCompare# i8 i9 of
          EQ -> pure (# len, len #)
          LT -> P.skipUntil '#' >> 
            let !urlFragmentStart = i9 +# urlPathStart
             in pure (# (i8 +# urlPathStart), urlFragmentStart #)
          GT -> P.skipUntil '#' >> 
            let !urlFragmentStart = i9 +# urlPathStart
             in pure (# urlFragmentStart, urlFragmentStart #)
      )
  >>= \(# !urlQueryStart, !urlFragmentStart #) ->
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
