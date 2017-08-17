{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- for FormatAsHex and FormatType
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#  define _OVERLAPPING_
#  define _OVERLAPPABLE_
#  define _OVERLAPS_
#else
#  define _OVERLAPPING_ {-# OVERLAPPING #-}
#  define _OVERLAPPABLE_ {-# OVERLAPPABLE #-}
#  define _OVERLAPS_ {-# OVERLAPS #-}
#endif

{- | A module providing access to internals (in case you really need them).
Can change at any time, though probably won't.

It also provides some functions that are used in 'Fmt.Time' (so that
'Fmt.Time' wouldn't need to import 'Fmt').
-}
module Fmt.Internal
(
  -- * Classes
  FromBuilder(..),
  FormatAsHex(..),
  FormatAsBase64(..),
  TupleF(..),

  -- * Classes used for 'genericF'
  GBuildable(..),
  GetFields(..),
  Buildable'(..),

  -- * Polyvariadic 'format'
  FormatType(..),
  
  -- * Helpers
  groupInt,
  atBase,
  showSigned',
  intToDigit',
  indent',

  -- * Functions used in 'Fmt.Time'
  fixedF,
  ordinalF,
)
where

-- Generic useful things
import Data.Monoid
import Numeric
import Data.Char
-- Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
-- 'Buildable' and text-format
import Data.Text.Buildable
import qualified Data.Text.Format as TF
-- Text 'Builder'
import Data.Text.Lazy.Builder hiding (fromString)
-- Bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- Formatting bytestrings
import qualified Data.ByteString.Base16          as B16
import qualified Data.ByteString.Base16.Lazy     as B16L
import qualified Data.ByteString.Base64          as B64
import qualified Data.ByteString.Base64.Lazy     as B64L
import qualified Data.ByteString.Base64.URL      as B64U
import qualified Data.ByteString.Base64.URL.Lazy as B64UL

----------------------------------------------------------------------------
-- FromBuilder
----------------------------------------------------------------------------

class FromBuilder a where
  -- | Convert a 'Builder' to something else.
  fromBuilder :: Builder -> a

instance FromBuilder Builder where
  fromBuilder = id
  {-# INLINE fromBuilder #-}

instance (a ~ Char) => FromBuilder [a] where
  fromBuilder = TL.unpack . toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder T.Text where
  fromBuilder = TL.toStrict . toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder TL.Text where
  fromBuilder = toLazyText
  {-# INLINE fromBuilder #-}

instance (a ~ ()) => FromBuilder (IO a) where
  fromBuilder = TL.putStr . toLazyText
  {-# INLINE fromBuilder #-}

----------------------------------------------------------------------------
-- Hex
----------------------------------------------------------------------------

class FormatAsHex a where
  {- |
Format a number or bytestring as hex:

>>> hexF 3635
"e33"
>>> hexF ("\0\50\63\80" :: BS.ByteString)
"00323f50"
  -}
  hexF :: a -> Builder

instance FormatAsHex BS.ByteString where
  hexF = fromText . T.decodeLatin1 . B16.encode

instance FormatAsHex BSL.ByteString where
  hexF = fromLazyText . TL.decodeLatin1 . B16L.encode

instance _OVERLAPPABLE_ Integral a => FormatAsHex a where
  hexF = TF.hex

----------------------------------------------------------------------------
-- Base64
----------------------------------------------------------------------------

class FormatAsBase64 a where
  {- |
Convert a bytestring to base64:

>>> base64F ("\0\50\63\80" :: BS.ByteString)
"ADI/UA=="
  -}
  base64F :: a -> Builder
  {- |
Convert a bytestring to base64url (a variant of base64 which omits @\/@ and
thus can be used in URLs):

>>> base64UrlF ("\0\50\63\80" :: BS.ByteString)
"ADI_UA=="
  -}
  base64UrlF :: a -> Builder

instance FormatAsBase64 BS.ByteString where
  base64F    = fromText . T.decodeLatin1 . B64.encode
  base64UrlF = fromText . T.decodeLatin1 . B64U.encode

instance FormatAsBase64 BSL.ByteString where
  base64F    = fromLazyText . TL.decodeLatin1 . B64L.encode
  base64UrlF = fromLazyText . TL.decodeLatin1 . B64UL.encode

----------------------------------------------------------------------------
-- Tuples
----------------------------------------------------------------------------

class TupleF a where
  {- |
Format a tuple (of up to 8 elements):

>>> tupleF (1,2,"hi")
"(1, 2, hi)"

If any of the elements takes several lines, an alternate format is used:

@
>>> __fmt $ tupleF ("test","foo\\nbar","more test")__
( test
,
  foo
  bar
,
  more test )
@
  -}
  tupleF :: a -> Builder

----------------------------------------------------------------------------
-- Classes used for 'genericF'
----------------------------------------------------------------------------

class GBuildable f where
  gbuild :: f a -> Builder

class GetFields f where
  -- | Get fields, together with their names if available
  getFields :: f a -> [(String, Builder)]

-- | A more powerful 'Buildable' used for 'genericF'. Can build functions,
-- tuples, lists, maps, etc., as well as combinations thereof.
class Buildable' a where
  build' :: a -> Builder

----------------------------------------------------------------------------
-- Classes used for polyvariadic 'format'
----------------------------------------------------------------------------

-- | Something like 'Text.Printf.PrintfType' in "Text.Printf".
class FormatType r where
  format' :: TF.Format -> [Builder] -> r

instance (Buildable a, FormatType r) => FormatType (a -> r) where
  format' f xs = \x -> format' f (build x : xs)

instance _OVERLAPPABLE_ FromBuilder r => FormatType r where
  format' f xs = fromBuilder $ TF.build f (reverse xs)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

groupInt :: (Buildable a, Integral a) => Int -> Char -> a -> Builder
groupInt 0 _ n = build n
groupInt i c n =
    fromLazyText . TL.reverse .
    foldr merge "" .
    TL.zip (zeros <> cycle' zeros') .
    TL.reverse .
    toLazyText . build
      $ n
  where
    zeros = TL.replicate (fromIntegral i) (TL.singleton '0')
    zeros' = TL.singleton c <> TL.tail zeros
    merge (f, c') rest
      | f == c = TL.singleton c <> TL.singleton c' <> rest
      | otherwise = TL.singleton c' <> rest
    cycle' xs = xs <> cycle' xs
    -- Suppress the warning about redundant Integral constraint
    _ = toInteger n

atBase :: Integral a => Int -> a -> String
atBase b _ | b < 2 || b > 36 = error ("baseF: Invalid base " ++ show b)
atBase b n =
  showSigned' (showIntAtBase (toInteger b) intToDigit') (toInteger n) ""
{-# INLINE atBase #-}

showSigned' :: Real a => (a -> ShowS) -> a -> ShowS
showSigned' f n
  | n < 0     = showChar '-' . f (negate n)
  | otherwise = f n

intToDigit' :: Int -> Char
intToDigit' i
  | i >= 0  && i < 10 = chr (ord '0' + i)
  | i >= 10 && i < 36 = chr (ord 'a' + i - 10)
  | otherwise = error ("intToDigit': Invalid int " ++ show i)

{- | Add a prefix to the first line, and indent all lines but the first one.

The output will always end with a newline, even when the input doesn't.
-}
indent' :: Int -> T.Text -> Builder -> Builder
indent' n pref a = case TL.lines (toLazyText a) of
  []     -> fromText pref <> "\n"
  (x:xs) -> fromLazyText $
            TL.unlines $ (TL.fromStrict pref <> x) : map (spaces <>) xs
  where
    spaces = TL.replicate (fromIntegral n) (TL.singleton ' ')

----------------------------------------------------------------------------
-- Functions used in Fmt.Time
----------------------------------------------------------------------------

{- |
Format a floating-point number without scientific notation:

>>> listF' (fixedF 5) [pi,0.1,10]
"[3.14159, 0.10000, 10.00000]"
-}
fixedF :: Real a => Int -> a -> Builder
fixedF = TF.fixed

{- |
Add an ordinal suffix to a number:

>>> ordinalF 15
"15th"
>>> ordinalF 22
"22nd"
-}
ordinalF :: (Buildable a, Integral a) => a -> Builder
ordinalF n
  | tens > 3 && tens < 21 = build n <> "th"
  | otherwise = build n <> case n `mod` 10 of
                             1 -> "st"
                             2 -> "nd"
                             3 -> "rd"
                             _ -> "th"
  where
    tens = n `mod` 100
