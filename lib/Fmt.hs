{-# LANGUAGE FlexibleInstances #-}

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

  -- ** Integers
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
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder hiding (fromString)
import Data.Monoid
import Data.Text.Buildable
import qualified Data.Text.Format as TF

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
                               (singleton '\n' <> go (TL.tail t'))

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
* commas, ords
* left, right, center, fitLeft, fitRight
* something to format bytestrings as hex
* 'time' that would use hackage.haskell.org/package/time/docs/Data-Time-Format.html#t:FormatTime
* something that would show time and date in a standard way
* conditional formatting (if x then y else mempty)
-}

-- TODO: mention that fmt doesn't do the neat thing that formatting does with (<>)
-- TODO: something to format a record nicely (with generics, probably)
-- TODO: something like https://hackage.haskell.org/package/groom
-- TODO: something for wrapping lists (not indenting, just hard-wrapping)
-- TODO: write docs
-- TODO: reexport (<>)?
-- TODO: write that if %< >% are hated, you can just use
--       provided formatters and <> (add Fmt.DIY for that?)
-- TODO: write that it can be used in parallel with formatting?
-- TODO: mention printf in cabal description so that it would be findable
-- TODO: mention things that work (<n+1>, <f n>, <show n>)
-- TODO: colors?
-- TODO: clarify philosophy (“take a free spot in design space; write the
--       best possible library around it, not just a proof of concept”)
-- TODO: clarify what exactly is hard about writing `formatting` formatters
-- TODO: add NL or _NL for newline? or (<\>) or (<>\)? and also (>%\)?
-- TODO: have to decide on whether it would be >%< or >%%< or maybe >|<
-- TODO: actually, what about |< and >|?
-- TODO: what effect does it have on compilation time? what effect do
--       other formatting libraries have on compilation time?

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
