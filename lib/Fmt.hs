{- Acknowledgements
~~~~~~~~~~~~~~~~~~~

* 'prefixF', 'suffixF', 'padCenterF', 'groupInt' are taken from
      <https://hackage.haskell.org/package/formatting>
  Written by Github user @mwm
      <https://github.com/mwm>

* 'ordinalF' is taken from
      <https://hackage.haskell.org/package/formatting>
  Written by Chris Done
      <https://github.com/chrisdone>

* 'atBase' is taken from
      <https://hackage.haskell.org/package/formatting>, originally from
      <https://hackage.haskell.org/package/lens>
  Seems to be written by Johan Kiviniemi
      <https://github.com/ion1>
-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

-- for FormatAsHex
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

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

  Builder,
  FromBuilder(..),
  Buildable(..),

  -- * Formatters
  indent, indent',
  nameF,

  -- ** Lists
  listF, listF',
  blockListF, blockListF',
  jsonListF, jsonListF',

  -- ** Maps
  mapF, mapF',
  blockMapF, blockMapF',
  jsonMapF, jsonMapF',

  -- ** Tuples
  tupleF,
  tupleLikeF,

  -- ** ADTs
  maybeF,
  eitherF,

  -- ** Padding/trimming
  prefixF,
  suffixF,
  padLeftF,
  padRightF,
  padCenterF,

  -- ** Hex
  hexF,

  -- ** Bytestrings
  base64F,

  -- ** Integers
  ordinalF,
  commaizeF,
  -- *** Base conversion
  octF,
  binF,
  baseF,

  -- ** Floating-point
  floatF,
  exptF,
  fixedF,
  precF,

  -- ** Conditional formatting
  whenF,
  unlessF,
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
import qualified Data.Text.Lazy.Encoding as TL
-- 'Buildable' and text-format
import Data.Text.Buildable
import qualified Data.Text.Format as TF
-- Text 'Builder'
import Data.Text.Lazy.Builder hiding (fromString)
-- 'Foldable' and 'IsList' for list/map formatters
import Data.Foldable (toList)
#if __GLASGOW_HASKELL__ >= 708
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IsList (toList)
#endif
-- Bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- Formatting bytestrings
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as B16L
import qualified Data.ByteString.Base64 as B64

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
#endif


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

nameF :: (Buildable a, Buildable b) => a -> b -> Builder
nameF k v = case TL.lines (toLazyText (build v)) of
    []  -> build k <> ":\n"
    [l] -> build k <> ": " <> fromLazyText l <> "\n"
    ls  -> build k <> ":\n" <>
           mconcat ["  " <> fromLazyText s <> "\n" | s <- ls]

----------------------------------------------------------------------------
-- List formatters
----------------------------------------------------------------------------

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

jsonListF :: forall f a. (Foldable f, Buildable a) => f a -> Builder
jsonListF = jsonListF' build
{-# INLINE jsonListF #-}

jsonListF' :: forall f a. (Foldable f) => (a -> Builder) -> f a -> Builder
jsonListF' fbuild xs
  | null items = "[]\n"
  | otherwise  = "[\n" <> mconcat items <> "]\n"
  where
    items = zipWith buildItem (True : repeat False) (toList xs)
    -- Item builder
    buildItem :: Bool -> a -> Builder
    buildItem isFirst x =
      case map fromLazyText (TL.lines (toLazyText (fbuild x))) of
        [] | isFirst   -> "\n"
           | otherwise -> ",\n"
        ls ->
            mconcat . map (<> "\n") $
              ls & _head %~ (if isFirst then ("  " <>) else (", " <>))
                 & _tail.each %~ ("  " <>)

{-# SPECIALIZE jsonListF' :: (a -> Builder) -> [a] -> Builder #-}

----------------------------------------------------------------------------
-- Map formatters
----------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 708
#  define MAPTOLIST IsList.toList
#else
#  define MAPTOLIST id
#endif

-- | Simple JSON-like map formatter; works for Map, HashMap, etc
mapF ::
#if __GLASGOW_HASKELL__ >= 708
    (IsList t, Item t ~ (k, v), Buildable k, Buildable v) => t -> Builder
#else
    (Buildable k, Buildable v) => [(k, v)] -> Builder
#endif
mapF = mapF' build build
{-# INLINE mapF #-}

mapF' ::
#if __GLASGOW_HASKELL__ >= 708
    (IsList t, Item t ~ (k, v)) =>
    (k -> Builder) -> (v -> Builder) -> t -> Builder
#else
    (k -> Builder) -> (v -> Builder) -> [(k, v)] -> Builder
#endif
mapF' fbuild_k fbuild_v xs =
  "{" <> mconcat (intersperse ", " (map buildPair (MAPTOLIST xs))) <> "}"
  where
    buildPair (k, v) = fbuild_k k <> ": " <> fbuild_v v

blockMapF ::
#if __GLASGOW_HASKELL__ >= 708
    (IsList t, Item t ~ (k, v), Buildable k, Buildable v) => t -> Builder
#else
    (Buildable k, Buildable v) => [(k, v)] -> Builder
#endif
blockMapF = blockMapF' build build
{-# INLINE blockMapF #-}

blockMapF' ::
#if __GLASGOW_HASKELL__ >= 708
    (IsList t, Item t ~ (k, v)) =>
    (k -> Builder) -> (v -> Builder) -> t -> Builder
#else
    (k -> Builder) -> (v -> Builder) -> [(k, v)] -> Builder
#endif
blockMapF' fbuild_k fbuild_v xs
  | null items = "{}\n"
  | otherwise  = mconcat items
  where
    items = map (\(k, v) -> nameF (fbuild_k k) (fbuild_v v)) (MAPTOLIST xs)

jsonMapF ::
#if __GLASGOW_HASKELL__ >= 708
    (IsList t, Item t ~ (k, v), Buildable k, Buildable v) => t -> Builder
#else
    (Buildable k, Buildable v) => [(k, v)] -> Builder
#endif
jsonMapF = jsonMapF' build build
{-# INLINE jsonMapF #-}

jsonMapF' ::
#if __GLASGOW_HASKELL__ >= 708
    forall t k v.
    (IsList t, Item t ~ (k, v)) =>
    (k -> Builder) -> (v -> Builder) -> t -> Builder
#else
    forall k v.
    (k -> Builder) -> (v -> Builder) -> [(k, v)] -> Builder
#endif
jsonMapF' fbuild_k fbuild_v xs
  | null items = "{}\n"
  | otherwise  = "{\n" <> mconcat items <> "}\n"
  where
    items = zipWith buildItem (True : repeat False) (MAPTOLIST xs)
    -- Item builder
    buildItem :: Bool -> (k, v) -> Builder
    buildItem isFirst (k, v) = do
      let kb = (if isFirst then "  " else ", ") <> fbuild_k k
      case map fromLazyText (TL.lines (toLazyText (fbuild_v v))) of
        []  -> kb <> ":\n"
        [l] -> kb <> ": " <> l <> "\n"
        ls  -> kb <> ":\n" <>
               mconcat ["    " <> s <> "\n" | s <- ls]

----------------------------------------------------------------------------
-- Tuple formatters
----------------------------------------------------------------------------

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
        [] | isFirst && isLast -> (False, "()\n")
           | isFirst           -> (False, "(\n")
           |            isLast -> (False, "  )\n")
           | otherwise         -> (False, "")
        ls ->
           (not (null (tail ls)),
            mconcat . map (<> "\n") $
              ls & _head %~ (if isFirst then ("( " <>) else ("  " <>))
                 & _tail.each %~ ("  " <>)
                 & _last %~ (if isLast then (<> " )") else id))

----------------------------------------------------------------------------
-- ADT formatters
----------------------------------------------------------------------------

{- |
Like 'build' for 'Maybe', but displays 'Nothing' as @<Nothing>@ instead of an empty string.

'build':

>>> build (Nothing :: Maybe Int)
""
>>> build (Just 1 :: Maybe Int)
"1"

'maybeF':

>>> maybeF (Nothing :: Maybe Int)
"<Nothing>"
>>> maybeF (Just 1 :: Maybe Int)
"1"
-}
maybeF :: Buildable a => Maybe a -> Builder
maybeF = maybe "<Nothing>" build

eitherF :: (Buildable a, Buildable b) => Either a b -> Builder
eitherF = either (\x -> "<Left:> " <> build x) (\x -> "<Right:> " <> build x)

----------------------------------------------------------------------------
-- Hex
----------------------------------------------------------------------------

class FormatAsHex a where
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
-- Other formatters
----------------------------------------------------------------------------

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

base64F :: BS.ByteString -> Builder
base64F = fromText . T.decodeLatin1 . B64.encode

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

octF :: Integral a => a -> Builder
octF = baseF 8

binF :: Integral a => a -> Builder
binF = baseF 2

baseF :: Integral a => Int -> a -> Builder
baseF numBase = build . atBase numBase

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

----------------------------------------------------------------------------
-- Conditional formatters
----------------------------------------------------------------------------

whenF :: Bool -> Builder -> Builder
whenF True  x = x
whenF False _ = mempty
{-# INLINE whenF #-}

unlessF :: Bool -> Builder -> Builder
unlessF False x = x
unlessF True  _ = mempty
{-# INLINE unlessF #-}

----------------------------------------------------------------------------
-- Utilities
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
                          else fromLazyText l <> singleton '\n' <>
                               go (TL.tail t')

-- assumes that the prefix doesn't contain newlines
indent' :: (FromBuilder b) => Int -> T.Text -> Builder -> b
indent' n pref a = fromBuilder (go True (toLazyText a))
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

----------------------------------------------------------------------------
-- FromBuilder
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- TODOs
----------------------------------------------------------------------------

{- add these:

* something that would cut a string by adding ellipsis to the center
* 'time' that would use hackage.haskell.org/package/time/docs/Data-Time-Format.html#t:FormatTime
* something that would show time and date in a standard way
* make it possible to use base64F with lazy bytestrings
* fmt and fmtLn? or format and formatLn?
-}

{- list/map:

* maybe add something like blockMapF_ and blockListF_ that would add
  a blank line automatically? or `---` and `:::` or something?
* should also add something to _not_ add a blank line between list
  entries (e.g. when they are 'name'd and can be clearly differentiated)
* should also add something that would truncate lists in the middle
  (and maybe not in the middle as well)
* the problem is that the user might want to combine them so I guess
  we can't make a separate combinator for each
-}

{- docs

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

{- others

* change indentation to always add newlines
* something to format a record nicely (with generics, probably)
* something like https://hackage.haskell.org/package/groom
* something for wrapping lists (not indenting, just hard-wrapping)
* reexport (<>)? don't know whether to use Semigroup or Monoid, though
* colors?
* should it be called 'listBlock' or 'blockList'?
* add NL or _NL for newline? or (<\>) or (<>\)? and also (>%\)?
* have to decide on whether it would be >%< or >%%< or maybe >|<
* actually, what about |< and >|?
* what effect does it have on compilation time? what effect do
  other formatting libraries have on compilation time?
* use 4 spaces instead of 2?
* change tuples to correspond to jsonList
* be consistent about newlines after tuples/maps/lists
-}
