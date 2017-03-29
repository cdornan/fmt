{- Acknowledgements
~~~~~~~~~~~~~~~~~~~

* 'prefixF', 'suffixF', 'padBothF', 'groupInt' are taken from
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

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

{-# OPTIONS_GHC -Wno-orphans #-}

module Fmt
(
  -- * Overloaded strings
  -- $overloadedstrings

  -- * Examples
  -- $examples

  -- * Migration guide from @formatting@
  -- $migration

  -- * Basic formatting
  -- $brackets

  -- ** Ordinary brackets
  -- $god1
  (<%),
  (%>),
  -- ** 'Show' brackets
  -- $god2
  (<<%),
  (%>>),
  -- ** Combinations
  -- $god3
  (%><%),
  (%>><<%),
  (%><<%),
  (%>><%),

  -- * Old-style formatting
  format,
  formatLn,
  TF.Format,

  -- * Helper functions
  fmt,
  fmtLn,

  Builder,
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
  padBothF,

  -- ** Hex
  hexF,

  -- ** Bytestrings
  base64F,
  base64UrlF,

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
  precF,
  fixedF,

  -- ** Conditional formatting
  whenF,
  unlessF,

  -- ** Generic formatting
  genericF,
)
where

-- Generic useful things
import Data.List
import Data.Monoid
import Lens.Micro
-- Containers
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Sequence (Seq)
-- Text
import qualified Data.Text.Lazy as TL
-- 'Buildable' and text-format
import Data.Text.Buildable
import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TF
-- Text 'Builder'
import Data.Text.Lazy.Builder hiding (fromString)
-- 'Foldable' and 'IsList' for list/map formatters
import Data.Foldable (toList)
#if __GLASGOW_HASKELL__ >= 708
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IsList (toList)
#endif
-- Generics
import GHC.Generics

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
#endif

#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty (NonEmpty)
#endif

import Fmt.Internal


{- $overloadedstrings

You need @OverloadedStrings@ enabled to use this library. There are three ways to do it:

  * __In GHCi:__ do @:set -XOverloadedStrings@.

  * __In a module:__ add @\{\-\# LANGUAGE OverloadedStrings \#\-\}@
    to the beginning of your module.

  * __In a project:__ add @OverloadedStrings@ to the @default-extensions@
    section of your @.cabal@ file.
-}

{- $examples

Here's a bunch of examples because some people learn better by looking at
examples.

Insert some variables into a string:

>>> let (a, b, n) = ("foo", "bar", 25)
>>> ("Here are some words: "<%a%>", "<%b%>"\nAlso a number: "<%n%>"") :: String
"Here are some words: foo, bar\nAlso a number: 25"

Print it:

>>> fmtLn ("Here are some words: "<%a%>", "<%b%>"\nAlso a number: "<%n%>"")
Here are some words: foo, bar
Also a number: 25

Format a list in various ways:

>>> let xs = ["John", "Bob"]

>>> fmtLn ("Using show: "<<%xs%>>"\nUsing listF: "<%listF xs%>"")
Using show: ["John","Bob"]
Using listF: [John, Bob]

>>> fmt ("YAML-like:\n"<%blockListF xs%>"")
YAML-like:
- John
- Bob

>>> fmt ("JSON-like: "<%jsonListF xs%>"")
JSON-like: [
  John
, Bob
]

-}

{- $migration

Instead of using @%@, surround variables with @<%@ and @%>@. You don't have
to use @sformat@ or anything else, and also where you were using @build@,
@int@, @text@, etc in @formatting@, you don't have to use anything in @fmt@:

@
__formatting__    sformat ("Foo: "%build%", bar: "%int) foo bar
       __fmt__    "Foo: "\<%foo%\>", bar: "\<%bar%\>""
@

The resulting formatted string is polymorphic and can be used as 'String',
'Text', 'Builder' or even 'IO' (i.e. the string will be printed to the
screen). However, when printing it is recommended to use 'fmt' or 'fmtLn' for
clarity.

@fmt@ provides lots of formatters (which are simply functions that produce
'Builder'):

@
__formatting__    sformat ("Got another byte ("%hex%")") x
       __fmt__    "Got another byte ("\<%hexF x%\>")"
@

Instead of the @shown@ formatter, either just use 'show' or double brackets:

@
__formatting__    sformat ("This uses Show: "%shown%") foo
    __fmt #1__    "This uses Show: "\<%show foo%\>""
    __fmt #2__    "This uses Show: "\<\<%foo%\>\>""
@

Many formatters from @formatting@ have the same names in @fmt@, but with
added “F”: 'hexF', 'exptF', etc. Some have been renamed, though:

@
__Cutting:__
  fitLeft  -\> 'prefixF'
  fitRight -\> 'suffixF'

__Padding:__
  left   -\> 'padLeftF'
  right  -\> 'padRightF'
  center -\> 'padBothF'

__Stuff with numbers:__
  ords   -\> 'ordinalF'
  commas -\> 'commaizeF'
@

Also, some formatters from @formatting@ haven't been added to @fmt@
yet. Specifically:

* @plural@ and @asInt@ (but instead of @asInt@ you can use 'fromEnum')
* @prefixBin@, @prefixOrd@, @prefixHex@, and @bytes@
* formatters that use @Scientific@ (@sci@ and @scifmt@)
* formatters that deal with time (anything from @Formatting.Time@)

They will be added later. (On the other hand, @fmt@ provides some useful
formatters not available in @formatting@, such as 'listF', 'mapF', 'tupleF'
and so on.)
-}

----------------------------------------------------------------------------
-- Operators with 'Buildable'
----------------------------------------------------------------------------

{- $brackets

To format strings, put variables between ('<%') and ('%>'):

>>> let name = "Alice"
>>> "Meet "<%name%>"!" :: String
"Meet Alice!"

Of course, 'Text' is supported as well:

>>> "Meet "<%name%>"!" :: Text
"Meet Alice!"

You don't actually need any type signatures; however, if you're toying with
this library in GHCi, it's recommended to either add a type signature or use
'fmtLn':

>>> fmtLn ("Meet "<%name%>"!")
Meet Alice!

Otherwise the type of the formatted string would be resolved to @IO ()@ and
printed without a newline, which is not very convenient when you're in
GHCi. On the other hand, it's useful for quick-and-dirty scripts:

@
main = do
  [fin, fout] \<- words \<$\> getArgs
  __"Reading data from "\<%fin%\>"\\n"__
  xs \<- readFile fin
  __"Writing processed data to "\<%fout%\>"\\n"__
  writeFile fout (show (process xs))
@

Anyway, let's proceed. Anything 'Buildable', including numbers, booleans,
characters and dates, can be put between ('<%') and ('%>'):

>>> let starCount = "173"
>>> fmtLn ("Meet "<%name%>"! She's got "<%starCount%>" stars on Github.")
"Meet Alice! She's got 173 stars on Github."

Since the only thing ('<%') and ('%>') do is concatenate strings and do
conversion, you can use any functions you want inside them. In this case,
'length':

>>> fmtLn (""<%name%>"'s name has "<%length name%>" letters")
Alice's name has 5 letters

If something isn't 'Buildable', just use 'show' on it:

>>> let pos = (3, 5)
>>> fmtLn ("Character's position: "<%show pos%>"")
Character's position: (3,5)

Or one of many formatters provided by this library – for instance, for tuples
of various sizes there's 'tupleF':

>>> fmtLn ("Character's position: "<%tupleF pos%>"")
Character's position: (3, 5)

Finally, for convenience there's the ('%><%') operator, which can be used if
you've got one variable following the other:

>>> let (a, op, b, res) = (2, "*", 2, 4)
>>> fmtLn (""<%a%><%op%><%b%>" = "<%res%>"")
2*2 = 4

Also, since in some codebases there are /lots/ of types which aren't
'Buildable', there are operators ('<<%') and ('%>>'), which use 'show'
instead of 'build':

@
(""\<%show foo%\>\<%show bar%\>"")    ===    (""\<\<%foo%\>\>\<\<%bar%\>\>"")
@
-}

-- $god1
-- Operators for the operators god!

-- | Concatenate, then convert
(<%) :: (FromBuilder b) => Builder -> Builder -> b
(<%) str rest = fromBuilder (str <> rest)

-- | 'build' and concatenate, then convert
(%>) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(%>) a rest = fromBuilder (build a <> rest)

infixr 1 <%
infixr 1 %>

----------------------------------------------------------------------------
-- Operators with 'Show'
----------------------------------------------------------------------------

-- $god2
-- More operators for the operators god!

-- | Concatenate, then convert
(<<%) :: (FromBuilder b) => Builder -> Builder -> b
(<<%) str rest = str <% rest
{-# INLINE (<<%) #-}

-- | 'show' and concatenate, then convert
(%>>) :: (Show a, FromBuilder b) => a -> Builder -> b
(%>>) a rest = show a %> rest
{-# INLINE (%>>) #-}

infixr 1 <<%
infixr 1 %>>

----------------------------------------------------------------------------
-- Combinations
----------------------------------------------------------------------------

{- $god3

Z̸͠A̵̕͟͠L̡̀́͠G̶̛O͝ ̴͏̀ I͞S̸̸̢͠  ̢̛͘͢C̷͟͡Ó̧̨̧͞M̡͘͟͞I̷͜N̷̕G̷̀̕

(Though you can just use @""@ between @%\> \<%@ instead of using these
operators, and Show-brackets don't have to be used at all because there's
'show' available.)
-}

(%><%) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(%><%) a rest = fromBuilder (build a <> rest)
{-# INLINE (%><%) #-}

(%>><<%) :: (Show a, FromBuilder b) => a -> Builder -> b
(%>><<%) a rest = show a %> rest
{-# INLINE (%>><<%) #-}

(%>><%) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(%>><%) a rest = a %><% rest
{-# INLINE (%>><%) #-}

(%><<%) :: (Show a, FromBuilder b) => a -> Builder -> b
(%><<%) a rest = a %>><<% rest
{-# INLINE (%><<%) #-}

infixr 1 %><%
infixr 1 %>><<%
infixr 1 %>><%
infixr 1 %><<%

----------------------------------------------------------------------------
-- Old-style formatting
----------------------------------------------------------------------------

{- | An old-style formatting function taken from @text-format@ (see
"Data.Text.Format"). Unlike 'Data.Text.Format.format' from
"Data.Text.Format", it can produce 'String' and strict 'Text' as well (and
print to console too).

To provide substitution arguments, use a tuple:

>>> format "{} + {} = {}" (2, 2, 4)
"2 + 2 = 4"

You can use arbitrary formatters:

>>> format "0x{} + 0x{} = 0x{}" (hexF 130, hexF 270, hexF (130+270))
"2 + 2 = 4"

To provide just one argument, use a list instead of a tuple:

>>> format "Hello {}!" ["world"]
"Hello world!"
-}
format :: (FromBuilder b, TF.Params ps) => TF.Format -> ps -> b
format f ps = fromBuilder (TF.build f ps)
{-# INLINE format #-}

{- | Like 'format', but adds a newline.
-}
formatLn :: (FromBuilder b, TF.Params ps) => TF.Format -> ps -> b
formatLn f ps = fromBuilder (TF.build f ps <> "\n")
{-# INLINE formatLn #-}

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

{- | 'fmt' converts things to 'String', 'Text' or 'Builder'.

Most of the time you won't need it, as strings produced with ('<%') and
('%>') can already be used as 'String', 'Text', etc. However, combinators
like 'listF' can only produce 'Builder' (for better type inference), and you
need to use 'fmt' on them.

Also, 'fmt' can do printing:

>>> fmt "Hello world!\n"
Hello world!
-}
fmt :: FromBuilder b => Builder -> b
fmt = fromBuilder
{-# INLINE fmt #-}

{- | Like 'fmt', but appends a newline.
-}
fmtLn :: FromBuilder b => Builder -> b
fmtLn = fromBuilder . (<> "\n")
{-# INLINE fmtLn #-}

----------------------------------------------------------------------------
-- Formatters
----------------------------------------------------------------------------

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
association-to-the-right is achieved when we avoid appending builders
until the last second (i.e. in the latter scenario):

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

It automatically handles multiline list elements:

@
>>> __fmt $ blockListF ["hello\\nworld", "foo\\nbar\\nquix"]__
- hello
  world

- foo
  bar
  quix
@
-}
blockListF :: forall f a. (Foldable f, Buildable a) => f a -> Builder
blockListF = blockListF' build
{-# INLINE blockListF #-}

{- | A version of 'blockListF' that lets you supply your own building function
for list elements.
-}
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

(Note that, unlike 'blockListF', it doesn't add blank lines in such cases.)
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
-- Tuple formatters
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

{- |
Format a list like a tuple. (This function is used to define 'tupleF'.)
-}
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

>>> eitherF (Right 1)
"<Right>: 1"
-}
eitherF :: (Buildable a, Buildable b) => Either a b -> Builder
eitherF = either (\x -> "<Left>: " <> build x) (\x -> "<Right>: " <> build x)

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
padLeftF = TF.left

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
padRightF = TF.right

{- |
@padBothF n c@ pads the string with character @c@ from both sides until
it becomes @n@ characters wide (and does nothing if the string is already
that long, or longer):

>>> padBothF 5 '=' "foo"
"=foo="
>>> padBothF 5 '=' "foobar"
"foobar"

When padding can't be distributed equally, the left side is preferred:

>>> padBoth 8 '=' "foo"
"===foo=="
-}
padBothF :: Buildable a => Int -> Char -> a -> Builder
padBothF i c =
  fromLazyText . TL.center (fromIntegral i) c . toLazyText . build

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

{- |
Break digits in a number:

>>> commaizeF 15830000
"15,830,000"
-}
commaizeF :: (Buildable a, Integral a) => a -> Builder
commaizeF = groupInt 3 ','

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
baseF :: Integral a => Int -> a -> Builder
baseF numBase = build . atBase numBase

{- |
Format a floating-point number:

>>> floatF 3.1415
"3.1415"

Numbers bigger than 1e21 or smaller than 1e-6 will be displayed using
scientific notation:

>>> listF' floatF [1e-6,9e-7]
"[0.000001, 9e-7]"
>>> listF' floatF [9e20,1e21]
"[900000000000000000000, 1e21]"
-}
floatF :: Real a => a -> Builder
floatF = TF.shortest

{- |
Format a floating-point number using scientific notation, with given amount
of precision:

>>> listF' (exptF 5) [pi,0.1,10]
"[3.14159e0, 1.00000e-1, 1.00000e1]"
-}
exptF :: Real a => Int -> a -> Builder
exptF = TF.expt

{- |
Format a floating-point number with given amount of precision.

For small numbers, it uses scientific notation for everything smaller than
1e-6:

>>> listF' (precF 3) [1e-5,1e-6,1e-7]
"[0.0000100, 0.00000100, 1.00e-7]"

For large numbers, it uses scientific notation for everything larger than
1eN, where N is the precision:

>>> listF' (precF 4) [1e3,5e3,1e4]
"[1000, 5000, 1.000e4]"
-}
precF :: Real a => Int -> a -> Builder
precF = TF.prec

{- |
Format a floating-point number without scientific notation:

>>> listF' (fixedF 5) [pi,0.1,10]
"[3.14159, 0.10000, 10.00000]"
-}
fixedF :: Real a => Int -> a -> Builder
fixedF = TF.fixed

----------------------------------------------------------------------------
-- Conditional formatters
----------------------------------------------------------------------------

{- |
Display something only if the condition is 'True' (empty string otherwise).

@
>>> __"Hello!" <> whenF showDetails (", details: "\<%foobar%\>"")__
@

Note that it can only take a 'Builder' (because otherwise it would be
unusable with ('<%')-formatted strings which can resolve to any 'FromBuilder'). Thus, use 'fmt' if you need just one value:

@
>>> __"Maybe here's a number: "\<%whenF cond (fmt n)%\>""__
@
-}
whenF :: Bool -> Builder -> Builder
whenF True  x = x
whenF False _ = mempty
{-# INLINE whenF #-}

{- |
Display something only if the condition is 'False' (empty string otherwise).
-}
unlessF :: Bool -> Builder -> Builder
unlessF False x = x
unlessF True  _ = mempty
{-# INLINE unlessF #-}

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

{- |
Indent already formatted text.

>>> fmt $ "This is a list:\n" <> indent 4 (blockListF [1,2,3])
This is a list:
    - 1
    - 2
    - 3

The output will always end with a newline, even when the input doesn't.
-}
indent :: Int -> Builder -> Builder
indent n a = case TL.lines (toLazyText a) of
    [] -> fromLazyText (spaces <> "\n")
    xs -> fromLazyText $ TL.unlines (map (spaces <>) xs)
  where
    spaces = TL.replicate (fromIntegral n) (TL.singleton ' ')

----------------------------------------------------------------------------
-- Generic formatting
----------------------------------------------------------------------------

{- | Format an arbitrary value without requiring a 'Buildable' instance:

@
data Foo = Foo { x :: Bool, y :: [Int] }
  deriving Generic
@

>>> fmt (genericF (Foo True [1,2,3]))
Foo:
  x: True
  y: [1, 2, 3]

It works for non-record constructors too:

@
data Bar = Bar Bool [Int]
  deriving Generic
@

>>> fmtLn (genericF (Bar True [1,2,3]))
<Bar: True, [1, 2, 3]>

Any fields inside the type must either be 'Buildable' or one of the following
types:

* a function
* a tuple (up to 8-tuples)
* list, 'NonEmpty', 'Seq'
* 'Map', 'IntMap', 'Set', 'IntSet'
* 'Maybe', 'Either'

The exact format of 'genericF' might change in future versions, so don't rely
on it. It's merely a convenience function.
-}
genericF :: (Generic a, GBuildable (Rep a)) => a -> Builder
genericF = gbuild . from

instance GBuildable a => GBuildable (M1 D d a) where
  gbuild (M1 x) = gbuild x

instance (GetFields a, Constructor c) => GBuildable (M1 C c a) where
  -- A note on fixity:
  --   * Ordinarily e.g. "Foo" is prefix and e.g. ":|" is infix
  --   * However, "Foo" can be infix when defined as "a `Foo` b"
  --   * And ":|" can be prefix when defined as "(:|) a b"
  gbuild c@(M1 x) = case conFixity c of
    Infix _ _
      | [a, b] <- fields -> format "({} {} {})" (a, infixName, b)
      -- this case should never happen, but still
      | otherwise        -> format "<{}: {}>"
                              ( prefixName
                              , mconcat (intersperse ", " fields) )
    Prefix
      | isTuple -> tupleLikeF fields
      | conIsRecord c -> nameF (build prefixName) (blockMapF fieldsWithNames)
      | null (getFields x) -> build prefixName
      -- I believe that there will be only one field in this case
      | null (conName c) -> mconcat (intersperse ", " fields)
      | otherwise -> format "<{}: {}>"
                       ( prefixName
                       , mconcat (intersperse ", " fields) )
    where
      (prefixName, infixName)
        | ":" `isPrefixOf` conName c = ("(" ++ conName c ++ ")", conName c)
        | otherwise                  = (conName c, "`" ++ conName c ++ "`")
      fields          = map snd (getFields x)
      fieldsWithNames = getFields x
      isTuple         = "(," `isPrefixOf` prefixName

instance Buildable' c => GBuildable (K1 i c) where
  gbuild (K1 a) = build' a

instance (GBuildable a, GBuildable b) => GBuildable (a :+: b) where
  gbuild (L1 x) = gbuild x
  gbuild (R1 x) = gbuild x

instance (GetFields a, GetFields b) => GetFields (a :*: b) where
  getFields (a :*: b) = getFields a ++ getFields b

instance (GBuildable a, Selector c) => GetFields (M1 S c a) where
  getFields s@(M1 a) = [(selName s, gbuild a)]

instance GBuildable a => GetFields (M1 D c a) where
  getFields (M1 a) = [("", gbuild a)]

instance GBuildable a => GetFields (M1 C c a) where
  getFields (M1 a) = [("", gbuild a)]

instance GetFields U1 where
  getFields _ = []

----------------------------------------------------------------------------
-- A more powerful Buildable used for genericF
----------------------------------------------------------------------------

instance Buildable' () where
  build' _ = "()"

instance (Buildable' a1, Buildable' a2)
  => Buildable' (a1, a2) where
  build' (a1, a2) = tupleLikeF
    [build' a1, build' a2]

instance (Buildable' a1, Buildable' a2, Buildable' a3)
  => Buildable' (a1, a2, a3) where
  build' (a1, a2, a3) = tupleLikeF
    [build' a1, build' a2, build' a3]

instance (Buildable' a1, Buildable' a2, Buildable' a3, Buildable' a4)
  => Buildable' (a1, a2, a3, a4) where
  build' (a1, a2, a3, a4) = tupleLikeF
    [build' a1, build' a2, build' a3, build' a4]

instance (Buildable' a1, Buildable' a2, Buildable' a3, Buildable' a4,
          Buildable' a5)
  => Buildable' (a1, a2, a3, a4, a5) where
  build' (a1, a2, a3, a4, a5) = tupleLikeF
    [build' a1, build' a2, build' a3, build' a4,
     build' a5]

instance (Buildable' a1, Buildable' a2, Buildable' a3, Buildable' a4,
          Buildable' a5, Buildable' a6)
  => Buildable' (a1, a2, a3, a4, a5, a6) where
  build' (a1, a2, a3, a4, a5, a6) = tupleLikeF
    [build' a1, build' a2, build' a3, build' a4,
     build' a5, build' a6]

instance (Buildable' a1, Buildable' a2, Buildable' a3, Buildable' a4,
          Buildable' a5, Buildable' a6, Buildable' a7)
  => Buildable' (a1, a2, a3, a4, a5, a6, a7) where
  build' (a1, a2, a3, a4, a5, a6, a7) = tupleLikeF
    [build' a1, build' a2, build' a3, build' a4,
     build' a5, build' a6, build' a7]

instance (Buildable' a1, Buildable' a2, Buildable' a3, Buildable' a4,
          Buildable' a5, Buildable' a6, Buildable' a7, Buildable' a8)
  => Buildable' (a1, a2, a3, a4, a5, a6, a7, a8) where
  build' (a1, a2, a3, a4, a5, a6, a7, a8) = tupleLikeF
    [build' a1, build' a2, build' a3, build' a4,
     build' a5, build' a6, build' a7, build' a8]

instance _OVERLAPPING_ Buildable' [Char] where
  build' = build

instance Buildable' a => Buildable' [a] where
  build' = listF' build'

#if MIN_VERSION_base(4,9,0)
instance Buildable' a => Buildable' (NonEmpty a) where
  build' = listF' build'
#endif

instance Buildable' a => Buildable' (Seq a) where
  build' = listF' build'

instance (Buildable' k, Buildable' v) => Buildable' (Map k v) where
  build' = mapF' build' build' . Map.toList

instance (Buildable' v) => Buildable' (Set v) where
  build' = listF' build'

instance (Buildable' v) => Buildable' (IntMap v) where
  build' = mapF' build' build' . IntMap.toList

instance Buildable' IntSet where
  build' = listF' build' . IntSet.toList

instance (Buildable' a) => Buildable' (Maybe a) where
  build' Nothing  = maybeF (Nothing         :: Maybe Builder)
  build' (Just a) = maybeF (Just (build' a) :: Maybe Builder)

instance (Buildable' a, Buildable' b) => Buildable' (Either a b) where
  build' (Left  a) = eitherF (Left  (build' a) :: Either Builder Builder)
  build' (Right a) = eitherF (Right (build' a) :: Either Builder Builder)

instance Buildable' (a -> b) where
  build' _ = "<function>"

instance _OVERLAPPABLE_ Buildable a => Buildable' a where
  build' = build

----------------------------------------------------------------------------
-- TODOs
----------------------------------------------------------------------------

{- add these
~~~~~~~~~~~~~~~~~~~~
* something that would cut a string by adding ellipsis to the center
* 'time' that would use hackage.haskell.org/package/time/docs/Data-Time-Format.html#t:FormatTime
* something that would show time and date in a standard way
* something to format a floating-point number without any scientific notation
-}

{- list/map
~~~~~~~~~~~~~~~~~~~~
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
~~~~~~~~~~~~~~~~~~~~
* write explicitly that 'build' can be used and is useful sometimes
* provide a formatting→fmt transition table
* mention that fmt doesn't do the neat thing that formatting does with (<>)
  (or maybe it does? there's a monoid instance for functions after all,
  though I might also have to write a IsString instance for (a -> Builder))
* write that if <% %> are hated or if it's inconvenient in some cases,
  you can just use provided formatters and <> (add Fmt.DIY for that?)
  (e.g. "pub:" <> base16F foo)
* write that it can be used in parallel with formatting?
* mention printf in cabal description so that it would be findable
* clarify philosophy (“take a free spot in design space; write the
  best possible library around it, not just a proof of concept”)
* clarify what exactly is hard about writing `formatting` formatters
-}

{- others
~~~~~~~~~~~~~~~~~~~~
* something for wrapping lists (not indenting, just hard-wrapping)
* reexport (<>)? don't know whether to use Semigroup or Monoid, though
* colors?
* should it be called 'listBlock' or 'blockList'?
* add NL or _NL for newline? or (<\>) or (<>\)? and also (%>\)?
* have to decide on whether it would be %|% or %><% or whatever
* actually, what about |< and >|?
* what effect does it have on compilation time? what effect do
  other formatting libraries have on compilation time?
* use 4 spaces instead of 2?
* change tuples to correspond to jsonList
* be consistent about newlines after tuples/maps/lists
* find some way to use IO inside <%%> brackets
-}
