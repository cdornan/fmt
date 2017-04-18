# fmt

[![Hackage](https://img.shields.io/hackage/v/fmt.svg)](https://hackage.haskell.org/package/fmt)
[![Build status](https://secure.travis-ci.org/aelve/fmt.svg)](https://travis-ci.org/aelve/fmt)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aelve/fmt/blob/master/LICENSE)

*A new formatting library. Doesn't try to be fancy. Also has some nice
formatters that other formatting libraries don't have.*

[fmt]: https://hackage.haskell.org/package/fmt

[`printf`]: http://hackage.haskell.org/package/base/docs/Text-Printf.html#v:printf

[formatting]: https://hackage.haskell.org/package/formatting
[text-format]: https://hackage.haskell.org/package/text-format

[category-printf]: https://hackage.haskell.org/package/category-printf
[xformat]: https://hackage.haskell.org/package/xformat

[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[interpolate]: https://hackage.haskell.org/package/interpolate

## About this library

The idea is very simple. Let's implement some formatters that are just
ordinary functions of type `a -> Builder`:

```haskell
> hexF 93
"5d"
```

And a bunch of operators (`#|`, `|#`) for concatenating strings:

```haskell
> let b = 93
> "Got another byte (0x"#|hexF b|#")"
"Got another byte (0x5d)"
```

And if we want to be able to produce `String`, `Text`, `Builder`, etc with
them, let's make those operators polymorphic in result type:

```haskell
> "Got another byte (0x"#|hexF b|#")" :: String
"Got another byte (0x5d)"

> "Got another byte (0x"#|hexF b|#")" :: Text
"Got another byte (0x5d)"
```

Finally, to make the library more useful, let's provide formatters that other
libraries often miss – for instance, something for indentation, something for
formatting lists and maps, and something to format arbitrary types with
generics:

```haskell
> data Point = Point {x, y :: Int} deriving Generic

> fmt $ genericF (Point 1 2)
Point:
  x: 1
  y: 2
```

If you want to see more examples, look at
the [docs](http://hackage.haskell.org/package/fmt/docs/Fmt.html).

## About other formatting libraries

Other commonly used libraries are [text-format][], [formatting][]
and [`printf`][] (which isn't a library but comes from `Text.Printf` in
base).

  * [`printf`][] takes a formatting string and uses some type tricks to
    accept the rest of the arguments polyvariadically. It's very concise, but
    there are some drawbacks – it can't produce `Text` (you'd have to
    `T.pack` it every time) and it doesn't warn you at compile-time if you
    pass wrong arguments or not enough of them.

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

## Benchmarks

The benchmark code can be
found [here](https://github.com/aelve/fmt/blob/master/bench/Main.hs). `fmt`
is usually twice as fast as `formatting`, and on par with `text-format`. This example was used for benchmarks:

```haskell
"There are "#|n|#" million bicycles in "#|city|#"."
```

<table>
  <thead><tr>
    <th>Library</th>
    <th>Text</th>
    <th>String</th
  </tr></thead>
  <tbody>
    <tr>
      <td>fmt</td>
      <td>0.4 mcs</td>
      <td>1.1 mcs</td>
    </tr>
    <tr>
      <td>formatting</td>
      <td>0.6 mcs</td>
      <td>1.4 mcs</td>
    </tr>
    <tr>
      <td>text-format</td>
      <td>0.3 mcs</td>
      <td>1.1 mcs</td>
    </tr>
    <tr>
      <td><code>printf</code></td>
      <td>2.0 mcs</td>
      <td>1.6 mcs</td>
    </tr>
    <tr>
      <td><code>show</code> and <code>&lt;&gt;</code></td>
      <td>0.9 mcs</td>
      <td>0.5 mcs</td>
    </tr>
  </tbody>
</table>

## Things to do

### Easy things to implement

1.  Time formatters. `formatting` has many of them and we have none.

2.  Something that would cut a string by adding ellipsis to the center:
    `Foo bar ba...qux blah`.

3.  Something to format a floating-point number without any scientific
    notation (`floatF` starts using scientific notation after 1e21).

4.  Write `RULES` to make it faster for `String` (and perhaps for `Text` as
    well).

### Questions to answer

1.  Should the operators remain `#|` and `|#`, or are there some better names?

2.  Is there any way to replace `#|` and `|#` with just one operator? (I
    tried but it doesn't seem to be possible if you want the library to keep
    working with enabled `-XOverloadedStrings`.)
   
3.  Should support for terminal coloring be added? If so, how should it be
    designed?

4.  Is there any way to allow `IO` inside `#| |#` brackets? (With the result
    being an `IO` action that prints the string.)

5.  How should `listF`, `mapF`, etc format lists/maps? Also, should we be
    more clever about multiline detection, or less clever (e.g. by providing
    something that will always use several lines and also something that will
    never use several lines)?  Currently `listF` doesnk't try to be clever at
    all, but perhaps it should.

6.  Can we somehow make formatters overloaded instead of always outputting
    `Builder`? Currently, if you want `listF xs` to be a `Text`, you have to
    write either `fmt (listF xs)` or `""#|listF xs|#""`, which doesn't seem
    nice. However, making formatters overloaded breaks type inference.

7.  It'd be nice to have something for wrapping lists (to fit into 80 chars
    or something), and also something for truncating lists (e.g. you might
    want long lists to be displayed as `[foo, bar, baz, ... (187 elements
    omitted)]`). This is hard because a) the design space is big, and b)
    there are too many combinations (`listTrimmedF`, `blockListTrimmedF`,
    `listTrimmedF'`, etc).

8.  By the way, is the `|##|` operator abhorrent and should be removed? What
    about `#||` and `||#`?

9.  How should tuples be formatted? I'm uneasy about the current syntax.

10. Should formatters be changed to never add a trailing newline?

11. Two spaces for list indentation, or four?

12. Should `genericF`'s syntax for constructors be changed?
