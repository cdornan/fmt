{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Fmt
(
  (%<),
  (>%),
  (>%%<),

  (%<<),
  (>>%),
  (>>%%<<),

  (>%%<<),
  (>>%%<),

  FromBuilder(..),

  Buildable(..),

  -- * Formatters
  indent,

  -- ** Padding/trimming
  prefixF,
  suffixF,
  padLeftF,
  padRightF,
  padCenterF,

  -- ** Bytestrings
  base16F,
  base64F,

  -- ** Integers
  ordinalF,
  commaizeF,
  -- *** Base conversion
  hexF,
  octF,
  binF,
  baseF,

  -- ** Floating-point
  floatF,
  exptF,
  fixedF,
  precF,
)
where

import Numeric
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder hiding (fromString)
import Data.Monoid
import Data.Text.Buildable
import qualified Data.Text.Format as TF
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64

----------------------------------------------------------------------------
-- Operators with 'Buildable'
----------------------------------------------------------------------------

(%<) :: (FromBuilder b) => Builder -> Builder -> b
(%<) str rest = fromBuilder (str <> rest)

(>%) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(>%) a rest = fromBuilder (build a <> rest)

(>%%<) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(>%%<) a rest = fromBuilder (build a <> rest)

infixr 1 %<
infixr 1 >%
infixr 1 >%%<

----------------------------------------------------------------------------
-- Operators with 'Show'
----------------------------------------------------------------------------

(%<<) :: (FromBuilder b) => Builder -> Builder -> b
(%<<) str rest = str %< rest
{-# INLINE (%<<) #-}

(>>%) :: (Show a, FromBuilder b) => a -> Builder -> b
(>>%) a rest = show a >% rest
{-# INLINE (>>%) #-}

(>>%%<<) :: (Show a, FromBuilder b) => a -> Builder -> b
(>>%%<<) a rest = show a >% rest
{-# INLINE (>>%%<<) #-}

infixr 1 %<<
infixr 1 >>%
infixr 1 >>%%<<

----------------------------------------------------------------------------
-- Combinations
----------------------------------------------------------------------------

(>>%%<) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(>>%%<) a rest = a >%%< rest
{-# INLINE (>>%%<) #-}

(>%%<<) :: (Show a, FromBuilder b) => a -> Builder -> b
(>%%<<) a rest = a >>%%<< rest
{-# INLINE (>%%<<) #-}

infixr 1 >>%%<
infixr 1 >%%<<

----------------------------------------------------------------------------
-- Formatters
----------------------------------------------------------------------------

indent :: (FromBuilder b) => Int -> Builder -> b
indent n a = fromBuilder (go (toLazyText a))
  where
    spaces = fromText (T.replicate n (T.singleton ' '))
    -- We don't use 'lines' because it doesn't distinguish between trailing
    -- newline being present/absent. We want the following behavior:
    --     >>> indent 2 "hi"
    --     "  hi"
    --     >>> indent 2 "hi\n"
    --     "  hi\n"
    go t | TL.null t = mempty
    go t = let (l, t') = TL.break ((==) '\n') t
           in spaces <> if TL.null t'
                          then fromLazyText l
                          else fromLazyText l <>
                               singleton '\n' <> go (TL.tail t')


-- | Fit in the given length, truncating on the left.
prefixF :: Buildable a => Int -> a -> Builder
prefixF size =
  fromLazyText . TL.take (fromIntegral size) . toLazyText . build

-- | Fit in the given length, truncating on the right.
suffixF :: Buildable a => Int -> a -> Builder
suffixF size =
  fromLazyText .
  (\t -> TL.drop (TL.length t - fromIntegral size) t) .
  toLazyText . build

-- | Pad the left hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
padLeftF :: Buildable a => Int -> Char -> a -> Builder
padLeftF = TF.left

-- | Pad the right hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
padRightF :: Buildable a => Int -> Char -> a -> Builder
padRightF = TF.right

-- | Pad the left & right hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
padCenterF :: Buildable a => Int -> Char -> a -> Builder
padCenterF i c =
  fromLazyText . TL.center (fromIntegral i) c . toLazyText . build

base16F :: BS.ByteString -> Builder
base16F = fromText . T.decodeLatin1 . B16.encode

base64F :: BS.ByteString -> Builder
base64F = fromText . T.decodeLatin1 . B64.encode

-- Taken from 'formatting'
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

commaizeF :: (Buildable a, Integral a) => a -> Builder
commaizeF = groupInt 3 ','

-- Taken from 'formatting'
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

hexF :: Integral a => a -> Builder
hexF = TF.hex

octF :: Integral a => a -> Builder
octF = baseF 8

binF :: Integral a => a -> Builder
binF = baseF 2

baseF :: Integral a => Int -> a -> Builder
baseF numBase = build . atBase numBase

-- The following code is taken from 'formatting' (which took it from
-- "Numeric.Lens" from 'lens').
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

floatF :: Real a => a -> Builder
floatF = TF.shortest

exptF :: Real a => Int -> a -> Builder
exptF = TF.expt

fixedF :: Real a => Int -> a -> Builder
fixedF = TF.fixed

precF :: Real a => Int -> a -> Builder
precF = TF.prec

{- TODO add these:
* something that would cut a string by adding ellipsis to the center
* 'time' that would use hackage.haskell.org/package/time/docs/Data-Time-Format.html#t:FormatTime
* something that would show time and date in a standard way
* conditional formatting (if x then y else mempty)
* optimise base16F and base64F
* make it possible to use base16F and base64F with lazy bytestrings?
* tupleF? or some other way to format pairs?
-}

{- DOCS TODOS

* mention that fmt doesn't do the neat thing that formatting does with (<>)
  (or maybe it does? there's a monoid instance for functions after all,
  though I might also have to write a IsString instance for (a -> Builder))
* write that if %< >% are hated or if it's inconvenient in some cases,
  you can just use provided formatters and <> (add Fmt.DIY for that?)
  (e.g. "pub:" <> base16F foo)
* write that it can be used in parallel with formatting?
* mention printf in cabal description so that it would be findable
* mention things that work (<n+1>, <f n>, <show n>)
* clarify philosophy (“take a free spot in design space; write the
  best possible library around it, not just a proof of concept”)
* clarify what exactly is hard about writing `formatting` formatters

-}

{- OTHER TODOS

* credit people properly in the comments (use git blame to find
  who wrote what code)
* something to format a record nicely (with generics, probably)
* something like https://hackage.haskell.org/package/groom
* something for wrapping lists (not indenting, just hard-wrapping)
* reexport (<>)?
* reexport 'Builder'
* colors?
* add NL or _NL for newline? or (<\>) or (<>\)? and also (>%\)?
* have to decide on whether it would be >%< or >%%< or maybe >|<
* actually, what about |< and >|?
* what effect does it have on compilation time? what effect do
  other formatting libraries have on compilation time?
-}

class FromBuilder a where
  fromBuilder :: Builder -> a

instance FromBuilder Builder where
  fromBuilder = id
  {-# INLINE fromBuilder #-}

instance FromBuilder String where
  fromBuilder = TL.unpack . toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder T.Text where
  fromBuilder = TL.toStrict . toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder TL.Text where
  fromBuilder = toLazyText
  {-# INLINE fromBuilder #-}
