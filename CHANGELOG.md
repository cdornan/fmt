# 0.0.0.3

* Added some formatters:

    * `indent`
    * formatters for lists and maps (`listF`, etc)
    * `hexF`, `octF`, `binF`, `baseF` and floating-point formatters
    * `ordinalF` and `commaizeF`
    * padding and trimming formatters
    * `base64F` and `base16F`

* Made all operators associate to the right (`Builder` documentation says it's faster than the opposite).

* Reexported `Buildable`.

# 0.0.0.2

* Added `>%%<` so that it'd be possible to write `%<a>%%<b>%` instead of weird `%<a%<b>%`.

* Added `%<< ... >>%`, which work work `Show` instead of `Buildable`. If you don't care about speed and just want to output something, use them.

* Added an `IO ()` instance in `Fmt.IO`. If you import that module, raw formatted strings would print themselves.

* Added tests.

* Changed fixities of operators so that `%<n+1>%` would work.

* Changed license to BSD3 since all our dependencies are BSD3 and we can't use MIT.

# 0.0.0.1

First (completely experimental) release.
