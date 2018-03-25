{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The 'Format' type, copied from "Data.Text.Format".
module Fmt.Internal.Format
( 
  Format(..),
  renderFormat,
)
where

import Data.String (IsString(..))
import Data.Text (Text, splitOn)
import Data.Text.Lazy.Builder hiding (fromString)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif

-- | A format string. This is intentionally incompatible with other
-- string types, to make it difficult to construct a format string by
-- concatenating string fragments (a very common way to accidentally
-- make code vulnerable to malicious data).
--
-- This type is an instance of 'IsString', so the easiest way to
-- construct a query is to enable the @OverloadedStrings@ language
-- extension and then simply write the query in double quotes.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Fmt
-- >
-- > f :: Format
-- > f = "hello {}"
--
-- The underlying type is 'Text', so literal Haskell strings that
-- contain Unicode characters will be correctly handled.
newtype Format = Format { fromFormat :: Text }
  deriving (Eq, Ord, Show)

instance Semigroup Format where
  Format a <> Format b = Format (a <> b)

instance Monoid Format where
  mempty = Format mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance IsString Format where
  fromString = Format . fromString

-- Format strings are almost always constants, and they're expensive
-- to interpret (which we refer to as "cracking" here).  We'd really
-- like to have GHC memoize the cracking of a known-constant format
-- string, so that it occurs at most once.
--
-- To achieve this, we arrange to have the cracked version of a format
-- string let-floated out as a CAF, by inlining the definitions of
-- build and functions that invoke it.  This works well with GHC 7.

-- | Render a format string and arguments to a 'Builder'.
renderFormat :: Format -> [Builder] -> Builder
renderFormat fmt ps = zipParams (crack fmt) ps
{-# INLINE renderFormat #-}

zipParams :: [Builder] -> [Builder] -> Builder
zipParams fragments params = go fragments params
  where go (f:fs) (y:ys) = f <> y <> go fs ys
        go [f] []        = f
        go _ _  = error $ "Fmt.format: there were " <> show (length fragments - 1) <>
                          " sites, but " <> show (length params) <> " parameters"

crack :: Format -> [Builder]
crack = map fromText . splitOn "{}" . fromFormat
