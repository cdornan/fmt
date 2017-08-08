# 0.4.0.0

* Renamed `#|` and `|#` to `+|` and `|+` because HLint can't handle `#|` and everyone uses HLint apparently.

# 0.3.0.0

* Added time formatters (see `Fmt.Time`).

# 0.2.0.0

* Changed `format` and `formatLn` to be polyvariadic.

# 0.1.0.0

* Added `genericF` for formatting arbitrary data.

* Changed `%<` and `>%` to `#|` and `|#` because they turn out to be easier to type.

* Added a migration guide from `formatting`.

* Changed output of `eitherF`.

* Added bechmarks.

# 0.0.0.4

* Added `format` from `text-format`, because in some cases it's nicer than brackets.

* Renamed `padCenterF` to `padBothF`.

* Modified `indent` and `indent'` to always add newlines.

# 0.0.0.3

* Wrote documentation.

* Added some formatters:

    * `indent`
    * formatters for lists, maps and tuples (`listF`, etc)
    * `octF`, `binF`, `baseF` and floating-point formatters
    * `hexF` which works on both numbers and bytestrings
    * `ordinalF` and `commaizeF`
    * padding and trimming formatters
    * `base64F` and `base64UrlF`
    * conditionals (`whenF` and `unlessF`)

* Merged `Fmt.IO` with `Fmt` because orphan instances are controversial.

* Exported internal classes and functions from `Fmt.Internal`.

* Added `fmt` and `fmtLn`.

* Made all operators associate to the right (`Builder` documentation says it's faster than the opposite).

* Reexported `Buildable` and `Builder`.

# 0.0.0.2

* Added `>%%<` so that it'd be possible to write `%<a>%%<b>%` instead of weird `%<a%<b>%`.

* Added `%<< ... >>%`, which work work `Show` instead of `Buildable`. If you don't care about speed and just want to output something, use them.

* Added an `IO ()` instance in `Fmt.IO`. If you import that module, raw formatted strings would print themselves.

* Added tests.

* Changed fixities of operators so that `%<n+1>%` would work.

* Changed license to BSD3 since all our dependencies are BSD3 and we can't use MIT.

# 0.0.0.1

First (completely experimental) release.
