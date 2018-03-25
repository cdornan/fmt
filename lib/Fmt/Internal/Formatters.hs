{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Fmt.Internal.Formatters where


-- Generic useful things
import Data.List
import Lens.Micro
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mconcat, mempty)
#endif
-- Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
-- 'Buildable' and text-format stuff
import Formatting.Buildable
import qualified Formatting.Internal.Raw as F
-- Text 'Builder'
import Data.Text.Lazy.Builder hiding (fromString)
-- 'Foldable' and 'IsList' for list/map formatters
import Data.Foldable (toList)
#if __GLASGOW_HASKELL__ >= 708
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IsList (toList)
#endif
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
#endif

import Fmt.Internal.Core


----------------------------------------------------------------------------
-- Text formatters
----------------------------------------------------------------------------

{- |
Indent a block of text.

>>> fmt $ "This is a list:\n" <> indentF 4 (blockListF [1,2,3])
This is a list:
    - 1
    - 2
    - 3

The output will always end with a newline, even when the input doesn't.
-}
indentF :: Int -> Builder -> Builder
indentF n a = case TL.lines (toLazyText a) of
    [] -> fromLazyText (spaces <> "\n")
    xs -> fromLazyText $ TL.unlines (map (spaces <>) xs)
  where
    spaces = TL.replicate (fromIntegral n) (TL.singleton ' ')

{- | Add a prefix to the first line, and indent all lines but the first one.

The output will always end with a newline, even when the input doesn't.
-}
indentF' :: Int -> T.Text -> Builder -> Builder
indentF' n pref a = case TL.lines (toLazyText a) of
  []     -> fromText pref <> "\n"
  (x:xs) -> fromLazyText $
            TL.unlines $ (TL.fromStrict pref <> x) : map (spaces <>) xs
  where
    spaces = TL.replicate (fromIntegral n) (TL.singleton ' ')

{- | Attach a name to anything:

>>> fmt $ nameF "clients" $ blockListF ["Alice", "Bob", "Zalgo"]
clients:
  - Alice
  - Bob
  - Zalgo
-}
nameF :: Builder -> Builder -> Builder
nameF k v = case TL.lines (toLazyText v) of
    []  -> k <> ":\n"
    [l] -> k <> ": " <> fromLazyText l <> "\n"
    ls  -> k <> ":\n" <>
           mconcat ["  " <> fromLazyText s <> "\n" | s <- ls]

{- | Put words between elements.

>>> fmt $ unwordsF ["hello", "world"]
hello world

Of course, it works on anything 'Buildable':

>>> fmt $ unwordsF [1, 2]
1 2
-}
unwordsF :: (Foldable f, Buildable a) => f a -> Builder
unwordsF = mconcat . intersperse " " . map build . toList

{-# SPECIALIZE unwordsF :: Buildable a => [a] -> Builder #-}

{- | Arrange elements on separate lines.

>>> fmt $ unlinesF ["hello", "world"]
hello
world
-}
unlinesF :: (Foldable f, Buildable a) => f a -> Builder
unlinesF = mconcat . map (nl . build) . toList
  where
    nl x | "\n" `TL.isSuffixOf` toLazyText x = x
         | otherwise = x <> "\n"

{-# SPECIALIZE unlinesF :: Buildable a => [a] -> Builder #-}

----------------------------------------------------------------------------
-- List formatters
----------------------------------------------------------------------------

{- | A simple comma-separated list formatter.

>>> listF ["hello", "world"]
"[hello, world]"

For multiline output, use 'jsonListF'.
-}
listF :: (Foldable f, Buildable a) => f a -> Builder
listF = listF' build
{-# INLINE listF #-}

{- | A version of 'listF' that lets you supply your own building function for
list elements.

For instance, to format a list of lists you'd have to do this (since there's
no 'Buildable' instance for lists):

>>> listF' listF [[1,2,3],[4,5,6]]
"[[1, 2, 3], [4, 5, 6]]"
-}
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
association-to-the-right is achieved when we avoid appending builders until
the last second (i.e. in the latter scenario):

    -- (a1 <> x) <> (a2 <> x) <> ...
    mconcat [a <> x | a <- as]

    -- a1 <> x <> a2 <> x <> ...
    mconcat $ concat [[a, x] | a <- as]

However, benchmarks have shown that the former way is actually faster.
-}

{- | A multiline formatter for lists.

>>> fmt $ blockListF [1,2,3]
- 1
- 2
- 3

Multi-line elements are indented correctly:

>>> fmt $ blockListF ["hello\nworld", "foo\nbar\nquix"]
- hello
  world
- foo
  bar
  quix

-}
blockListF :: forall f a. (Foldable f, Buildable a) => f a -> Builder
blockListF = blockListF' "-" build
{-# INLINE blockListF #-}

{- | A version of 'blockListF' that lets you supply your own building function
for list elements (instead of 'build') and choose the bullet character
(instead of @"-"@).
-}
blockListF'
  :: forall f a. Foldable f
  => Text                       -- ^ Bullet
  -> (a -> Builder)             -- ^ Builder for elements
  -> f a                        -- ^ Structure with elements
  -> Builder
blockListF' bullet fbuild xs = if null items then "[]\n" else mconcat items
  where
    items = map buildItem (toList xs)
    spaces = mconcat $ replicate (T.length bullet + 1) (singleton ' ')
    buildItem x = case TL.lines (toLazyText (fbuild x)) of
      []     -> bullet |+ "\n"
      (l:ls) -> bullet |+ " " +| l |+ "\n" <>
                mconcat [spaces <> fromLazyText s <> "\n" | s <- ls]

{-# SPECIALIZE blockListF' :: Text -> (a -> Builder) -> [a] -> Builder #-}

{- | A JSON-style formatter for lists.

>>> fmt $ jsonListF [1,2,3]
[
  1
, 2
, 3
]

Like 'blockListF', it handles multiline elements well:

>>> fmt $ jsonListF ["hello\nworld", "foo\nbar\nquix"]
[
  hello
  world
, foo
  bar
  quix
]
-}
jsonListF :: forall f a. (Foldable f, Buildable a) => f a -> Builder
jsonListF = jsonListF' build
{-# INLINE jsonListF #-}

{- | A version of 'jsonListF' that lets you supply your own building function
for list elements.
-}
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

{- | A simple JSON-like map formatter; works for Map, HashMap, etc, as well as
ordinary lists of pairs.

>>> mapF [("a", 1), ("b", 4)]
"{a: 1, b: 4}"

For multiline output, use 'jsonMapF'.
-}
mapF ::
#if __GLASGOW_HASKELL__ >= 708
    (IsList t, Item t ~ (k, v), Buildable k, Buildable v) => t -> Builder
#else
    (Buildable k, Buildable v) => [(k, v)] -> Builder
#endif
mapF = mapF' build build
{-# INLINE mapF #-}

{- | A version of 'mapF' that lets you supply your own building function for
keys and values.
-}
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

{- | A YAML-like map formatter:

>>> fmt $ blockMapF [("Odds", blockListF [1,3]), ("Evens", blockListF [2,4])]
Odds:
  - 1
  - 3
Evens:
  - 2
  - 4
-}
blockMapF ::
#if __GLASGOW_HASKELL__ >= 708
    (IsList t, Item t ~ (k, v), Buildable k, Buildable v) => t -> Builder
#else
    (Buildable k, Buildable v) => [(k, v)] -> Builder
#endif
blockMapF = blockMapF' build build
{-# INLINE blockMapF #-}

{- | A version of 'blockMapF' that lets you supply your own building function
for keys and values.
-}
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

{- | A JSON-like map formatter (unlike 'mapF', always multiline):

>>> fmt $ jsonMapF [("Odds", jsonListF [1,3]), ("Evens", jsonListF [2,4])]
{
  Odds:
    [
      1
    , 3
    ]
, Evens:
    [
      2
    , 4
    ]
}
-}
jsonMapF ::
#if __GLASGOW_HASKELL__ >= 708
    (IsList t, Item t ~ (k, v), Buildable k, Buildable v) => t -> Builder
#else
    (Buildable k, Buildable v) => [(k, v)] -> Builder
#endif
jsonMapF = jsonMapF' build build
{-# INLINE jsonMapF #-}

{- | A version of 'jsonMapF' that lets you supply your own building function
for keys and values.
-}
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
-- ADT formatters
----------------------------------------------------------------------------

{- | Like 'build' for 'Maybe', but displays 'Nothing' as @\<Nothing\>@ instead
of an empty string.

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

{- |
Format an 'Either':

>>> eitherF (Right 1 :: Either Bool Int)
"<Right: 1>"
-}
eitherF :: (Buildable a, Buildable b) => Either a b -> Builder
eitherF = either (\x -> "<Left: " <> build x <> ">")
                 (\x -> "<Right: " <> build x <> ">")

----------------------------------------------------------------------------
-- Other formatters
----------------------------------------------------------------------------

{- |
Take the first N characters:

>>> prefixF 3 "hello"
"hel"
-}
prefixF :: Buildable a => Int -> a -> Builder
prefixF size =
  fromLazyText . TL.take (fromIntegral size) . toLazyText . build

{- |
Take the last N characters:

>>> suffixF 3 "hello"
"llo"
-}
suffixF :: Buildable a => Int -> a -> Builder
suffixF size =
  fromLazyText .
  (\t -> TL.drop (TL.length t - fromIntegral size) t) .
  toLazyText . build

{- |
@padLeftF n c@ pads the string with character @c@ from the left side until it
becomes @n@ characters wide (and does nothing if the string is already that
long, or longer):

>>> padLeftF 5 '0' 12
"00012"
>>> padLeftF 5 '0' 123456
"123456"
-}
padLeftF :: Buildable a => Int -> Char -> a -> Builder
padLeftF = F.left

{- |
@padRightF n c@ pads the string with character @c@ from the right side until
it becomes @n@ characters wide (and does nothing if the string is already
that long, or longer):

>>> padRightF 5 ' ' "foo"
"foo  "
>>> padRightF 5 ' ' "foobar"
"foobar"
-}
padRightF :: Buildable a => Int -> Char -> a -> Builder
padRightF = F.right

{- |
@padBothF n c@ pads the string with character @c@ from both sides until
it becomes @n@ characters wide (and does nothing if the string is already
that long, or longer):

>>> padBothF 5 '=' "foo"
"=foo="
>>> padBothF 5 '=' "foobar"
"foobar"

When padding can't be distributed equally, the left side is preferred:

>>> padBothF 8 '=' "foo"
"===foo=="
-}
padBothF :: Buildable a => Int -> Char -> a -> Builder
padBothF i c =
  fromLazyText . TL.center (fromIntegral i) c . toLazyText . build

----------------------------------------------------------------------------
-- Conditional formatters
----------------------------------------------------------------------------

{- | Display something only if the condition is 'True' (empty string
otherwise).

Note that it can only take a 'Builder' (because otherwise it would be
unusable with ('+|')-formatted strings which can resolve to any
'FromBuilder'). You can use 'build' to convert any value to a 'Builder'.
-}
whenF :: Bool -> Builder -> Builder
whenF True  x = x
whenF False _ = mempty
{-# INLINE whenF #-}

{- | Display something only if the condition is 'False' (empty string
otherwise).
-}
unlessF :: Bool -> Builder -> Builder
unlessF False x = x
unlessF True  _ = mempty
{-# INLINE unlessF #-}
