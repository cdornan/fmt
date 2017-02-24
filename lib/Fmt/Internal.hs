{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- for FormatAsHex
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

{- | A module providing access to internals (in case you really need them). Can
change at any time, though probably won't.
-}
module Fmt.Internal
(
  -- * Classes
  FromBuilder(..),
  FormatAsHex(..),
  FormatAsBase64(..),

  -- * Helpers
  groupInt,
  atBase,
  showSigned',
  intToDigit',
  indent',
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
  -}
  hexF :: a -> Builder

instance FormatAsHex BS.ByteString where
  hexF = fromText . T.decodeLatin1 . B16.encode

instance FormatAsHex BSL.ByteString where
  hexF = fromLazyText . TL.decodeLatin1 . B16L.encode

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPABLE #-} Integral a => FormatAsHex a where
  hexF = TF.hex
#else
instance Integral a => FormatAsHex a where
  hexF = TF.hex
#endif

----------------------------------------------------------------------------
-- Base64
----------------------------------------------------------------------------

class FormatAsBase64 a where
  {- |
Convert a bytestring to base64:

>>> base64F ("\0\50\63\80" :: BS.ByteString)
"ADI/UA=="
  -}
  base64F    :: a -> Builder
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

-- assumes that the prefix doesn't contain newlines
indent' :: Int -> T.Text -> Builder -> Builder
indent' n pref a = go True (toLazyText a)
  where
    spaces = fromText (T.replicate n (T.singleton ' '))
    go isFirst t
      | TL.null t = if isFirst then fromText pref else ""
      | otherwise = let (l, t') = TL.break ((==) '\n') t
                    in (if isFirst then fromText pref else spaces) <>
                        if TL.null t'
                          then fromLazyText l
                          else fromLazyText l <> singleton '\n' <>
                               go False (TL.tail t')
