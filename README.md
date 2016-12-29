# fmt

[![Hackage](https://img.shields.io/hackage/v/fmt.svg)](https://hackage.haskell.org/package/fmt)
[![Build status](https://secure.travis-ci.org/aelve/fmt.svg)](https://travis-ci.org/aelve/fmt)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aelve/fmt/blob/master/LICENSE)

*A new formatting library. Fast, powerful, simple to use, contains no magic.*

[fmt]: https://hackage.haskell.org/package/fmt

## A comparison with other libraries

First of all: if you're trying to choose a formatting library, syntax should be your starting point, as you likely wouldn't want to use a formatting library if you hate its syntax. [citation needed] That's why I'm going to start with an example of [fmt][]'s syntax (note that it's the full string, no `format` or `printf` needed):

``` haskell
"There are "%<n>%" million bicycles in "%<city>%"."
```

If you hate it already, it's okay! I don't like it much either, to be honest. But if you don't *hate* it, keep reading.

Let's say that `n = 9` and `city = "Beijing"`. First, here's how we could produce “There are 9 million bicycles in Beijing.” only with things that are in base:

| Method           | Code                                                               |
| ---------------- | ------------------------------------------------------------------ |
| `(++)`           | `"There are " ++ show n ++ " million bicycles in " ++ city ++ "."` |
| `concat`         | `concat ["There are ", show n, " million bicycles in", city, "."]` |
| `Text.Printf`    | `printf "There are %d million bicycles in %s." n city`             |

[text-format]: https://hackage.haskell.org/package/text-format
[formatting]: https://hackage.haskell.org/package/formatting

The first two are bad for obvious reasons:

  * having to write `show` is annoying
  * `String` is kinda slow
  * there are few “built-in” formatters, and existing ones (`showFFloat`, etc) are cumbersome to use
  
`printf` is actually pretty good and concise, but it's not typesafe and doesn't produce `Text`, so in many cases using it is either annoying or forbidden. ~Which is a pity because I love `printf`.~

Next come [text-format][] and [formatting][], one from Bryan O'Sullivan, another from Chris Done. Based on a very representative survey (sample size = 2) I conducted among industry professionals (i.e. my friends), [formatting][] is the most popular formatting library used in Production™, so let's start with it.

| Library          | Code                                                                  |
| ---------------- | --------------------------------------------------------------------- |
| [formatting][]   | `sformat ("There are "%int%" million bicycles in "%build%".") n city` |
| [text-format][]  | `format "There are {} million bicycles in {}." (n, city)`             |

[formatting][] is type-safe and uses in-place formatters. What I have found is that writing your own formatters is somewhat non-obvious, and composing them in non-trivial ways is even more non-obvious, which is my main gripe about formatting – I don't understand how it works and I often have to go through “format things manually with `<>` and `build`, then stick it into the string” if I want to do something more-or-less complicated. The only thing `Format` gives me is the ability to put arguments after `sformat` instead of putting them between string pieces – and it's actually questionable whether it's a good thing or not! (Not to mention that if my variables are long enough that putting them between string pieces looks bad, perhaps I should've used `let` anyway.)

A nice thing about text-format is that it's as compact as `printf`. (However, it becomes less compact when you want to output floating numbers or add leading zeroes, because instead of format specifiers (`%.3f`, etc) it uses functions like `fixed`.) A not-nice thing about text-format is that, like `printf`, it isn't typesafe and is thus automatically banned from Production™. I probably sound a bit bitter. Sorry.

There are some other libraries, e.g. [category-printf][] and [xformat][]. They don't seem better or more powerful than [formatting][] to me, but admittedly I haven't looked at them in depth. [xformat][] in particular seems to run into a problem with ambiguous types when `OverloadedStrings` is enabled.

[category-printf]: https://hackage.haskell.org/package/category-printf
[xformat]: https://hackage.haskell.org/package/xformat

| Library             | Code                                                                  |
| ------------------- | --------------------------------------------------------------------- |
| [category-printf][] | `printf ("There are ".s." million bicycles in ".i.".") n city`        |
| [xformat][]         | `showf ("There are "%Int%" million bicycles in "%String%".") n city`  |

Finally, there's a cottage industry of libraries using Template Haskell (they tend to have “interpolation” in their names rather than “format” or “printf”). One bad thing about all of them is that it's pretty hard to use GHC's parser in Template Haskell and so either they have to do with interpolating variables only, or call into a separate Haskell parser (e.g. [haskell-src-exts][]). Most don't bother.

[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts

[interpolate]: https://hackage.haskell.org/package/interpolate

| Library             | Code                                               |
| ------------------- | -------------------------------------------------- |
| [interpolate][]     | `[i|There are #{n} million bicycles in #{city}.|]` |

I like TH-based formatting libraries, especially since they have the nicest syntax for multiline strings, but unfortunately they tend to be unusable as formatting libraries (no utilities for floating-point printing or left/right alignment or whatever) and they don't play well with editors (simplest example: the syntax highlighter has no idea that `n` in a quasi-quote is a variable).

Okay, where does this all leave us? [formatting][] is hard to use in non-trivial situations, and the rest are either worse/not-better than [formatting][] or not typesafe (and thus you can't use them even if you like them).

Perhaps I should've just figured out how to use [formatting][] and write a nice tutorial for it. However, exploring the design space is more fun (and *much* easier to when the library is your own). (Also, writing `sformat` everywhere sucks because I never know how to indent it properly. That's actually the main reason why I wrote this library, but I'll never admit it.)

Ahem.

## The idea

What if we just made an operator that `show`ed everything and `(++)`-ed it at the same time?

``` haskell
(%) :: Show a => String -> a -> String
(%) x a = x ++ show a
```

``` haskell
> "There are "%n%" million bicycles in "%city%"."
"There are 9 million bicycles in \"Beijing\"."
```

[`Buildable`]: https://hackage.haskell.org/package/text-format/docs/Data-Text-Buildable.html

Okay, well. What if we just made an operator that did the same thing but used [`Buildable`][]?

``` haskell
(%) :: Buildable a => Builder -> a -> Builder
(%) x a = x <> build a
```

``` haskell
> "There are "%n%" million bicycles in "%city%"."
"There are 9 million bicycles in Beijing."
```

Nice. What if we also wrote a hackish typeclass to make it output anything we want (`String`, `Text`, etc)?

``` haskell
class FromBuilder a where
  fromBuilder :: Builder -> a

instance FromBuilder Builder where fromBuilder = id
instance FromBuilder String  where fromBuilder = TL.unpack . toLazyText
instance FromBuilder T.Text  where fromBuilder = TL.toStrict . toLazyText
instance FromBuilder TL.Text where fromBuilder = toLazyText
```

``` haskell
(%) :: (Buildable a, FromBuilder b) => Builder -> a -> b
(%) x a = fromBuilder (x <> build a)
```

``` haskell
> "There are "%n%" million bicycles in "%city%"." :: String
"There are 9 million bicycles in Beijing."
```

Excellent. Except that it wouldn't compile outside of GHCi because in the absence of `-XExtendedDefaultRules` the compiler wouldn't know whether `" million bicycles in "` is supposed to be a `Text` or `String` or `Builder` or what.

What if we tried to help it by replacing `%` with a pair of synonyms – `%<` and `>%` – and required `>%` to always have `Builder` as its right argument? Then, if the user writes `"a"%<b>%"c"`, the compiler would know that `"c"` has to be a `Builder`.

``` haskell
> "There are "%<n>%" million bicycles in "%<city>%"." :: String
"There are 9 million bicycles in Beijing."
```

And it even looks kinda nicer.

What if we want to have two variables in a row? `%<a%<b>%` doesn't look good. Okay, let's add another operator for such cases, `>%%<`:

``` haskell
> "Some nines: "%<n>%%<n>%%<n>%"."
"Some nines: 999."
```

Looks a bit like magic, but I'm going to let it slide. (By the way, should this operator be `>%%<` or `>%<`, or maybe even `>|<`? If you have good arguments for one of these, tell me.)

Can we use other operators in `%<...>%` “brackets”? We can if we give `%<` and `>%` low precedence, e.g. `1` (not `0` because it should be usable with `$`):

``` haskell
> "Not nines: "%<n-2>%%<n-1>%%<n>%"."
"Not nines: 789."
```

[template-haskell]: https://hackage.haskell.org/package/template-haskell

The next problem is that lots of types don't have a `Buildable` instance, e.g. `Name` from [template-haskell][] (or even `(,)`). I could write my own class, but it'd be much nicer to keep compatibility with [formatting][] as lots of people already have `Buildable` instances written. I could just make users write `show` whenever they want to do it, but it'd be better to add a pair of operators for `Show`-able types:

``` haskell
> "A tuple: "%<<(n,n-1)>>%"."
"A tuple: (8,9)."
```

It means that I also have to add a small zoo of extra operators for consistency – `>>%<<`, `>%<<`, `>>%<` – but I hope that nobody would actually notice them in code.

One last thing is that if there's a variable in the beginning or at the end of string, you have to do one of these (not particularly elegant) things:

``` haskell
> "Number "%<n
"Number 9"

> "Number "%<n>%""
"Number 9"
```

I recommend the second one, but if you can think of better alternatives, please tell me.

(One last-last thing: if you have two string pieces in a row, because e.g. you had to break a string into several lines, just use `<>` there.)

## Extra features

### Effortless `putStr`

One of the usecases of `printf` is throwaway scripts. Marketing speak on: to capture `printf`'s marketshare, I need to provide a viable solution to those users' business needs. Marketing speak off: it'd be kinda cool to add a `FromBuilder (IO ())` instance so that people would be able to just write strings in `do` and they'd print as if by magic.

Marketing speak on again: but I don't want to lose my core users who value type safety and would likely be alienated by such tricks! Marketing speak off again: okay, I'll just put it into a separate module.

``` haskell
import Fmt.IO

main = do
  ...
  "Found "%<length xs>%" objects. Proceeding..."
  ...
```

I wouldn't recommend it over `printf`, as – at least for me – typing `"%<` and `>%"` is slightly annoying and I wouldn't want to do it in throwaway scripts. However, it was easy enough to implement and so I added it anyway. (If someone ends up using it, that's great.)

### Indentation, multiline formatting, generics

When I'm outputting a debug message, I often include all possible data there (just in case I need it). Since I can't be bothered to add indentation or anything, the end result is usually awful and I suffer.

There are several things in [fmt][] that help with that. Well, currently there's just one (`indent`), but there'll be more! The library is still a work in progress.
