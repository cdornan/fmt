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
  (+|),
  (|+),
  -- ** 'Show' brackets
  -- $god2
  (+||),
  (||+),
  -- ** Combinations
  -- $god3
  (|++|),
  (||++||),
  (|++||),
  (||++|),

  -- * Old-style formatting
  format,
  formatLn,
  Format,

  -- * Helper functions
  fmt, fmtLn,
  pretty, prettyLn,

  Builder,
  Buildable(..),

  -- * Formatters

  -- ** Time
  module Fmt.Time,

  -- ** Text
  indentF, indentF',
  nameF,
  unwordsF,
  unlinesF,

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
  fixedF,

  -- ** Conditional formatting
  whenF,
  unlessF,

  -- ** Generic formatting
  genericF,
  GenericBuildable(..),
)
where


import Formatting.Buildable (Buildable(..))
import Data.Text.Lazy.Builder

import Fmt.Internal
import Fmt.Time


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDeriveGeneric
-- >>> import Data.Text (Text)

{- $overloadedstrings

You need @OverloadedStrings@ enabled to use this library. There are three
ways to do it:

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
>>> ("Here are some words: "+|a|+", "+|b|+"\nAlso a number: "+|n|+"") :: String
"Here are some words: foo, bar\nAlso a number: 25"

Print it:

>>> fmtLn ("Here are some words: "+|a|+", "+|b|+"\nAlso a number: "+|n|+"")
Here are some words: foo, bar
Also a number: 25

Format a list in various ways:

>>> let xs = ["John", "Bob"]

>>> fmtLn ("Using show: "+||xs||+"\nUsing listF: "+|listF xs|+"")
Using show: ["John","Bob"]
Using listF: [John, Bob]

>>> fmt ("YAML-like:\n"+|blockListF xs|+"")
YAML-like:
- John
- Bob

>>> fmt ("JSON-like: "+|jsonListF xs|+"")
JSON-like: [
  John
, Bob
]

-}

{- $migration

Instead of using @%@, surround variables with '+|' and '|+'. You don't have
to use @sformat@ or anything else, and also where you were using @build@,
@int@, @text@, etc in @formatting@, you don't have to use anything in @fmt@:

@
formatting    __sformat ("Foo: "%build%", bar: "%int) foo bar__
       fmt    __"Foo: "+|foo|+", bar: "+|bar|+""__
@

The resulting formatted string is polymorphic and can be used as 'String',
'Text', 'Builder' or even 'IO' (i.e. the string will be printed to the
screen). However, when printing it is recommended to use 'fmt' or 'fmtLn'
for clarity.

@fmt@ provides lots of formatters (which are simply functions that produce
'Builder'):

@
formatting    __sformat ("Got another byte ("%hex%")") x__
       fmt    __"Got another byte ("+|hexF x|+")"__
@

Instead of the @shown@ formatter, either just use 'show' or double brackets:

@
formatting    __sformat ("This uses Show: "%shown%") foo__
    fmt #1    __"This uses Show: "+|show foo|+""__
    fmt #2    __"This uses Show: "+||foo||+""__
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

They will be added later. (On the other hand, @fmt@ provides some useful
formatters not available in @formatting@, such as 'listF', 'mapF', 'tupleF'
and so on.)
-}

----------------------------------------------------------------------------
-- Documentation for operators
----------------------------------------------------------------------------

{- $brackets

To format strings, put variables between ('+|') and ('|+'):

>>> let name = "Alice" :: String
>>> "Meet "+|name|+"!" :: String
"Meet Alice!"

Of course, 'Text' is supported as well:

>>> "Meet "+|name|+"!" :: Text
"Meet Alice!"

You don't actually need any type signatures; however, if you're toying with
this library in GHCi, it's recommended to either add a type signature or use
'fmtLn':

>>> fmtLn ("Meet "+|name|+"!")
Meet Alice!

Otherwise the type of the formatted string would be resolved to @IO ()@ and
printed without a newline, which is not very convenient when you're in GHCi.
On the other hand, it's useful for quick-and-dirty scripts:

@
main = do
  [fin, fout] \<- words \<$\> getArgs
  __"Reading data from "+|fin|+"\\n"__
  xs \<- readFile fin
  __"Writing processed data to "+|fout|+"\\n"__
  writeFile fout (show (process xs))
@

Anyway, let's proceed. Anything 'Buildable', including numbers, booleans,
characters and dates, can be put between ('+|') and ('|+'):

>>> let starCount = "173"
>>> fmtLn ("Meet "+|name|+"! She's got "+|starCount|+" stars on Github.")
Meet Alice! She's got 173 stars on Github.

Since the only thing ('+|') and ('|+') do is concatenate strings and do
conversion, you can use any functions you want inside them. In this case,
'length':

>>> fmtLn (""+|name|+"'s name has "+|length name|+" letters")
Alice's name has 5 letters

If something isn't 'Buildable', just use 'show' on it:

>>> let pos = (3, 5)
>>> fmtLn ("Character's position: "+|show pos|+"")
Character's position: (3,5)

Or one of many formatters provided by this library – for instance, for tuples
of various sizes there's 'tupleF':

>>> fmtLn ("Character's position: "+|tupleF pos|+"")
Character's position: (3, 5)

Finally, for convenience there's the ('|++|') operator, which can be used if
you've got one variable following the other:

>>> let (a, op, b, res) = (2, "*", 2, 4)
>>> fmtLn (""+|a|++|op|++|b|+" = "+|res|+"")
2*2 = 4

Also, since in some codebases there are /lots/ of types which aren't
'Buildable', there are operators ('+||') and ('||+'), which use 'show'
instead of 'build':

prop> (""+|show foo|++|show bar|+"") == (""+||foo||++||bar||+"")
-}

-- $god1
-- Operators for the operators god!

-- $god2
-- More operators for the operators god!

{- $god3

Z̸͠A̵̕͟͠L̡̀́͠G̶̛O͝ ̴͏̀ I͞S̸̸̢͠  ̢̛͘͢C̷͟͡Ó̧̨̧͞M̡͘͟͞I̷͜N̷̕G̷̀̕

(Though you can just use @""@ between @+| |+@ instead of using these
operators, and 'Show'-brackets don't have to be used at all because there's
'show' available.)
-}

----------------------------------------------------------------------------
-- TODOs
----------------------------------------------------------------------------

{- docs
~~~~~~~~~~~~~~~~~~~~
* mention that fmt doesn't do the neat thing that formatting does with (<>)
  (or maybe it does? there's a monoid instance for functions after all,
  though I might also have to write a IsString instance for (a -> Builder))
* write that if +| |+ are hated or if it's inconvenient in some cases,
  you can just use provided formatters and <> (add Fmt.DIY for that?)
  (e.g. "pub:" <> base16F foo)
* write that it can be used in parallel with formatting? can it, actually?
* clarify what exactly is hard about writing `formatting` formatters
-}

{- others
~~~~~~~~~~~~~~~~~~~~
* what effect does it have on compilation time? what effect do
  other formatting libraries have on compilation time?
-}
