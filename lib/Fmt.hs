{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
  nameF,

  -- ** Lists
  listF, listF',
  blockListF, blockListF',

  -- ** Maps
  mapF, mapF',
  blockMapF, blockMapF',

  -- ** Tuples
  tupleF,
  tupleLikeF,

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

-- Generic useful things
import Data.List
import Data.Char
import Data.Monoid
import Lens.Micro
import Numeric
-- Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
-- 'Buildable' and text-format
import Data.Text.Buildable
import qualified Data.Text.Format as TF
-- Text 'Builder'
import Data.Text.Lazy.Builder hiding (fromString)
-- 'Foldable' and 'IsList' for list/map formatters
import Data.Foldable (toList)
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IsList (toList)
-- Bytestring
import qualified Data.ByteString as BS
-- Formatting bytestrings
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

nameF :: (Buildable a, Buildable b) => a -> b -> Builder
nameF k v = case TL.lines (toLazyText (build v)) of
    []  -> build k <> ":\n"
    [l] -> build k <> ": " <> fromLazyText l <> "\n"
    ls  -> build k <> ":\n" <>
           mconcat ["  " <> fromLazyText s <> "\n" | s <- ls]

-- | Simple comma-separated list formatter
listF :: (Foldable f, Buildable a) => f a -> Builder
listF = listF' build
{-# INLINE listF #-}

listF' :: (Foldable f) => (a -> Builder) -> f a -> Builder
listF' fbuild xs = mconcat $
  "[" :
  intersperse ", " (map fbuild (toList xs)) ++
  ["]"]

{-# SPECIALIZE listF' :: (a -> Builder) -> [a] -> Builder #-}

{- Note [Builder appending]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The documentation for 'Builder' says that it's preferrable to associate
'Builder' appends to the right (i.e. @a <> (b <> c)@). The maximum possible
association-to-the-right is achieved when we avoid appending builders
until the last second (i.e. in the latter scenario):

    -- (a1 <> x) <> (a2 <> x) <> ...
    mconcat [a <> x | a <- as]

    -- a1 <> x <> a2 <> x <> ...
    mconcat $ concat [[a, x] | a <- as]

However, benchmarks have shown that the former way is actually faster.
-}

blockListF :: forall f a. (Foldable f, Buildable a) => f a -> Builder
blockListF = blockListF' build
{-# INLINE blockListF #-}

blockListF' :: forall f a. (Foldable f) => (a -> Builder) -> f a -> Builder
blockListF' fbuild xs
  | null items      = "[]\n"
  | True `elem` mls = mconcat (intersperse "\n" items)
  | otherwise       = mconcat items
  where
    (mls, items) = unzip $ map buildItem (toList xs)
    -- Returns 'True' if the item is multiline
    buildItem :: a -> (Bool, Builder)
    buildItem x = case TL.lines (toLazyText (fbuild x)) of
      []     -> (False, "-\n")
      (l:ls) -> (not (null ls),
                 "- " <> fromLazyText l <> "\n" <>
                     mconcat ["  " <> fromLazyText s <> "\n" | s <- ls])

{-# SPECIALIZE blockListF' :: (a -> Builder) -> [a] -> Builder #-}

-- | Simple JSON-like map formatter; works for Map, HashMap, etc
mapF :: (IsList t, Item t ~ (k, v), Buildable k, Buildable v)
     => t -> Builder
mapF = mapF' build build
{-# INLINE mapF #-}

mapF' :: (IsList t, Item t ~ (k, v))
      => (k -> Builder) -> (v -> Builder) -> t -> Builder
mapF' fbuild_k fbuild_v xs =
  "{" <> mconcat (intersperse ", " (map buildPair (IsList.toList xs))) <> "}"
  where
    buildPair (k, v) = fbuild_k k <> ": " <> fbuild_v v

blockMapF :: (IsList t, Item t ~ (k, v), Buildable k, Buildable v)
          => t -> Builder
blockMapF = blockMapF' build build
{-# INLINE blockMapF #-}

blockMapF' :: (IsList t, Item t ~ (k, v))
           => (k -> Builder) -> (v -> Builder) -> t -> Builder
blockMapF' fbuild_k fbuild_v xs
  | null items = "{}\n"
  | otherwise  = mconcat items
  where
    items = map (\(k, v) -> nameF (fbuild_k k) (fbuild_v v)) (IsList.toList xs)

-- TODO:
--   • maybe add something like blockMapF_ and blockListF_ that would add
--     a blank line automatically? or `---` and `:::` or something?
--   • should also add something to _not_ add a blank line between list
--     entries (e.g. when they are 'name'd and can be clearly differentiated)
--   • should also add something that would truncate lists in the middle
--     (and maybe not in the middle as well)
--   • the problem is that the user might want to combine them so I guess
--     we can't make a separate combinator for each

class TupleF a where
  tupleF :: a -> Builder

instance (Buildable a1, Buildable a2)
  => TupleF (a1, a2) where
  tupleF (a1, a2) = tupleLikeF
    [build a1, build a2]

instance (Buildable a1, Buildable a2, Buildable a3)
  => TupleF (a1, a2, a3) where
  tupleF (a1, a2, a3) = tupleLikeF
    [build a1, build a2, build a3]

instance (Buildable a1, Buildable a2, Buildable a3, Buildable a4)
  => TupleF (a1, a2, a3, a4) where
  tupleF (a1, a2, a3, a4) = tupleLikeF
    [build a1, build a2, build a3, build a4]

instance (Buildable a1, Buildable a2, Buildable a3, Buildable a4,
          Buildable a5)
  => TupleF (a1, a2, a3, a4, a5) where
  tupleF (a1, a2, a3, a4, a5) = tupleLikeF
    [build a1, build a2, build a3, build a4,
     build a5]

instance (Buildable a1, Buildable a2, Buildable a3, Buildable a4,
          Buildable a5, Buildable a6)
  => TupleF (a1, a2, a3, a4, a5, a6) where
  tupleF (a1, a2, a3, a4, a5, a6) = tupleLikeF
    [build a1, build a2, build a3, build a4,
     build a5, build a6]

instance (Buildable a1, Buildable a2, Buildable a3, Buildable a4,
          Buildable a5, Buildable a6, Buildable a7)
  => TupleF (a1, a2, a3, a4, a5, a6, a7) where
  tupleF (a1, a2, a3, a4, a5, a6, a7) = tupleLikeF
    [build a1, build a2, build a3, build a4,
     build a5, build a6, build a7]

instance (Buildable a1, Buildable a2, Buildable a3, Buildable a4,
          Buildable a5, Buildable a6, Buildable a7, Buildable a8)
  => TupleF (a1, a2, a3, a4, a5, a6, a7, a8) where
  tupleF (a1, a2, a3, a4, a5, a6, a7, a8) = tupleLikeF
    [build a1, build a2, build a3, build a4,
     build a5, build a6, build a7, build a8]

-- | Format a list like a tuple. Used to define 'tupleF'.
tupleLikeF :: [Builder] -> Builder
tupleLikeF xs
  | True `elem` mls = mconcat (intersperse ",\n" items)
  | otherwise = "(" <> mconcat (intersperse ", " xs) <> ")"
  where
    (mls, items) = unzip $ zipWith3 buildItem
                             xs (set _head True falses) (set _last True falses)
    -- A list of 'False's which has the same length as 'xs'
    falses = map (const False) xs
    -- Returns 'True' if the item is multiline
    buildItem :: Builder
              -> Bool              -- ^ Is the item the first?
              -> Bool              -- ^ Is the item the last?
              -> (Bool, Builder)
    buildItem x isFirst isLast =
      case map fromLazyText (TL.lines (toLazyText x)) of
        [] | isFirst && isLast -> (False, "()")
           | isFirst           -> (False, "(\n")
           |            isLast -> (False, "  )\n")
        ls ->
           (not (null (tail ls)),
            mconcat . map (<> "\n") $
              ls & _head %~ (if isFirst then ("( " <>) else ("  " <>))
                 & _tail.each %~ ("  " <>)
                 & _last %~ (if isLast then (<> " )") else id))

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
* add something for indenting all lines except for the first one?
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
* write that [(a,b)] works too and could be used
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
* add tests for tupleF and tupleLikeF
* use 4 spaces instead of 2?
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
