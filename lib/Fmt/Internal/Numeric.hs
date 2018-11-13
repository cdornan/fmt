{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}


module Fmt.Internal.Numeric where


import           Data.CallStack
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid ((<>))
#endif
import           Numeric
import           Data.Char
import           Data.Text.Lazy.Builder hiding (fromString)
import           Formatting.Buildable (Buildable(..))
import qualified Formatting.Internal.Raw as F
import qualified Data.Text.Lazy as TL


-- $setup
-- >>> import Fmt

----------------------------------------------------------------------------
-- Integer
----------------------------------------------------------------------------

{- |
Format a number as octal:

>>> listF' octF [7,8,9,10]
"[7, 10, 11, 12]"
-}
octF :: Integral a => a -> Builder
octF = baseF 8

{- |
Format a number as binary:

>>> listF' binF [7,8,9,10]
"[111, 1000, 1001, 1010]"
-}
binF :: Integral a => a -> Builder
binF = baseF 2

{- |
Format a number in arbitrary base (up to 36):

>>> baseF 3 10000
"111201101"
>>> baseF 7 10000
"41104"
>>> baseF 36 10000
"7ps"
-}
baseF :: (HasCallStack, Integral a) => Int -> a -> Builder
baseF numBase = build . atBase numBase

----------------------------------------------------------------------------
-- Floating-point
----------------------------------------------------------------------------

{- |
Format a floating-point number:

>>> floatF 3.1415
"3.1415"

Numbers smaller than 1e-6 or bigger-or-equal to 1e21 will be displayed using
scientific notation:

>>> listF' floatF [1e-6,9e-7]
"[0.000001, 9.0e-7]"
>>> listF' floatF [9e20,1e21]
"[900000000000000000000.0, 1.0e21]"
-}
floatF :: Real a => a -> Builder
floatF a | d < 1e-6 || d >= 1e21 = build $ showEFloat Nothing d ""
         | otherwise             = build $ showFFloat Nothing d ""
  where d = realToFrac a :: Double

{- | Format a floating-point number using scientific notation, with the given
amount of decimal places.

>>> listF' (exptF 5) [pi,0.1,10]
"[3.14159e0, 1.00000e-1, 1.00000e1]"
-}
exptF :: Real a => Int -> a -> Builder
exptF decs a = build $ showEFloat (Just decs) (realToFrac a :: Double) ""

{- |
Format a floating-point number without scientific notation:

>>> listF' (fixedF 5) [pi,0.1,10]
"[3.14159, 0.10000, 10.00000]"
-}
fixedF :: Real a => Int -> a -> Builder
fixedF = F.fixed

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

{- |
Break digits in a number:

>>> commaizeF 15830000
"15,830,000"
-}
commaizeF :: (Buildable a, Integral a) => a -> Builder
commaizeF = groupInt 3 ','

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
