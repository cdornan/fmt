# fmt

[![Hackage](https://img.shields.io/hackage/v/fmt.svg)](https://hackage.haskell.org/package/fmt)
[![Build status](https://secure.travis-ci.org/aelve/fmt.svg)](https://travis-ci.org/aelve/fmt)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aelve/fmt/blob/master/LICENSE)

*A new formatting library. Doesn't try to be fancy or use holey monoids. Has
some nice features that other formatting libraries don't have.*

[fmt]: https://hackage.haskell.org/package/fmt

[formatting]: https://hackage.haskell.org/package/formatting
[text-format]: https://hackage.haskell.org/package/text-format

[category-printf]: https://hackage.haskell.org/package/category-printf
[xformat]: https://hackage.haskell.org/package/xformat

[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[interpolate]: https://hackage.haskell.org/package/interpolate

## About `fmt`

`fmt` provides formatters that are just ordinary functions of type `a ->
Builder`, and a bunch of operators for concatenating formatted strings:

```haskell
> let b = 93
> "Got another byte ("0x<%hexF b%>")"
"Got another byte (0x5d)"
```

(Some find the operator syntax annoying, while others like it. If you find it
annoying, perhaps this library is not for you!)

Those operators also do automatic conversion to `Text`/`String`/`Builder`, so
you don't need to use `sformat` or something else to convert the formatted
string to the type you need.

A major difference from other libraries is that `fmt` provides more
convenience formatters – for instance, you can do indentation, or pretty-print
lists/tuples/maps (and even arbitrary types with generics).

## About other formatting libraries

Other commonly used libraries are [text-format][], [formatting][] and
`printf` (which isn't a library but comes from `Text.Printf` in base):

  * `printf` takes a formatting string and uses some type tricks to accept
    the rest of the arguments polyvariadically. It's very concise, but there
    are some drawbacks – it can't produce `Text` (you'd have to `T.pack` it
    every time) and it doesn't warn you at compile-time if you pass wrong
    arguments or not enough of them.

  * [text-format][] takes a formatting string with angle brackets denoting
    places where arguments would be substituted (the arguments themselves are
    provided via a tuple). If you want to apply formatting to some of the
    arguments, you have to use one of the provided formatters. Like `printf`,
    it can fail at runtime, but at least the formatters are first-class (and
    you can add new ones).

  * [formatting][] takes a formatting template consisting of pieces of
    strings interleaved with formatters (this ensures that arguments always
    match their placeholders). It provides lots of formatters and generally
    seems to be the most popular formatting library here. Unfortunately, at
    least in my experience writing new formatters can be awkward and people
    sometimes have troubles understanding how `formatting` works.

There are also some other libraries, e.g. [category-printf][]
and [xformat][]. They don't seem better or more powerful than [formatting][]
to me, but admittedly I haven't looked at them in depth. [xformat][] in
particular seems to run into a problem with ambiguous types when
`OverloadedStrings` is enabled.

Finally, there's a cottage industry of libraries using Template Haskell (they
tend to have “interpolation” in their names rather than “format” or
“printf”). One bad thing about all of them is that you can't use GHC's parser
in Template Haskell and so either they have to do with interpolating
variables only, or call into a separate Haskell parser
(e.g. [haskell-src-exts][], but most don't bother). Here's an example of
formatting done with [interpolate][]:

``` haskell
[i|There are #{n} million bicycles in #{city}.|]
```

## The origin story of `fmt`

What if we just made an operator that `show`ed everything and `(++)`-ed it at
the same time?

``` haskell
(%) :: Show a => String -> a -> String
(%) x a = x ++ show a
```

``` haskell
> "There are "%n%" million bicycles in "%city%"."
"There are 9 million bicycles in \"Beijing\"."
```

[`Buildable`]: https://hackage.haskell.org/package/text-format/docs/Data-Text-Buildable.html

Okay, well. What if we just made an operator that did the same thing but
used [`Buildable`][], since everyone else uses `Buildable`?

``` haskell
(%) :: Buildable a => Builder -> a -> Builder
(%) x a = x <> build a
```

``` haskell
> "There are "%n%" million bicycles in "%city%"."
"There are 9 million bicycles in Beijing."
```

What if we also wrote a hackish typeclass to make it output anything we want
(`String`, `Text`, etc)?

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

Now it wouldn't compile outside of GHCi because in the absence of
`-XExtendedDefaultRules` the compiler wouldn't know whether `" million
bicycles in "` is supposed to be a `Text` or `String` or `Builder` or what;
however, we can help it by replacing `%` with a pair of synonyms – `<%` and
`%>` – and required `%>` to always have `Builder` as its right argument.
Then, if the user writes `"a"<%x%>"b"`, the compiler would know that `"b"`
has to be a `Builder`.

``` haskell
> "There are "<%n%>" million bicycles in "<%city%>"." :: String
"There are 9 million bicycles in Beijing."
```

What if we want to have two variables in a row? `<%a<%b%>` doesn't look
good. Okay, let's add another operator for such cases, `%><%`:

``` haskell
> "Some nines: "<%n%><%n%><%n%>"."
"Some nines: 999."
```

Looks a bit like magic, but I'm going to let it slide.

Can we use other operators in `<%...%>` “brackets”? We can if we give `<%`
and `%>` low precedence, e.g. `1` (not `0` because it should be usable with
`$`):

``` haskell
> "Not nines: "<%n-2%><%n-1%><%n%>"."
"Not nines: 789."
```

[template-haskell]: https://hackage.haskell.org/package/template-haskell

The next problem is that lots of types don't have a `Buildable` instance,
e.g. `Name` from [template-haskell][] (or even `(,)`). I could write my own
class, but it'd be much nicer to keep compatibility with [formatting][] as
lots of people already have `Buildable` instances written. I could just make
users write `show` whenever they want to do it, but it'd be better to add a
pair of operators for `Show`-able types:

``` haskell
> "A tuple: "<<%(n,n-1)%>>"."
"A tuple: (8,9)."
```

It means that I also have to add a small zoo of extra operators for
consistency – `%>><<%`, `%><<%`, `%>><%` – but I hope that nobody would
actually notice them in code.

One last thing is that if there's a variable in the beginning or at the end
of string, you have to do one of these (not particularly elegant) things:

``` haskell
> "Number "<%n
"Number 9"

> "Number "<%n%>""
"Number 9"
```

I recommend the second one, but if you can think of better alternatives,
please tell me.

(One last-last thing: if you have two string pieces in a row, because
e.g. you had to break a string into several lines, just use `<>` there.)
