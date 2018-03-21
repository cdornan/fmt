{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | A module implementing functionality from 
Data.Text.Format
-}
module Fmt.Internal.Format
( 
  -- * Classes
  Format(..),
  Only(..),
  Params(..),

  -- * Helpers
  build,
  format,

  -- ** Floating-point formatters
  expt,
  prec,

  -- * Re-exports from Data.Text.Format
  module Formatting.Internal.Raw 
) where

import qualified Data.Double.Conversion.Text as C
import Data.String (IsString(..))
import Data.Text (Text, splitOn)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder hiding (fromString)
import Formatting.Buildable (Buildable)
import qualified Formatting.Buildable as B
import Formatting.Internal.Raw hiding ((<>))
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
-- > import Data.Text.Format
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

-- | Use this @newtype@ wrapper for your single parameter if you are
-- formatting a string containing exactly one substitution site.
newtype Only a = Only { fromOnly :: a }
  deriving (Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, Floating, RealFloat, Enum, Integral, Bounded)

-- Format strings are almost always constants, and they're expensive
-- to interpret (which we refer to as "cracking" here).  We'd really
-- like to have GHC memoize the cracking of a known-constant format
-- string, so that it occurs at most once.
--
-- To achieve this, we arrange to have the cracked version of a format
-- string let-floated out as a CAF, by inlining the definitions of
-- build and functions that invoke it.  This works well with GHC 7.

-- | Render a format string and arguments to a 'Builder'.
build :: Params ps => Format -> ps -> Builder
build fmt ps = zipParams (crack fmt) (buildParams ps)
{-# INLINE build #-}

zipParams :: [Builder] -> [Builder] -> Builder
zipParams fragments params = go fragments params
  where go (f:fs) (y:ys) = f <> y <> go fs ys
        go [f] []        = f
        go _ _ = error . LT.unpack $ format
                 "Data.Text.Format.build: {} sites, but {} parameters"
                 (length fragments - 1, length params)

crack :: Format -> [Builder]
crack = map fromText . splitOn "{}" . fromFormat

-- | Render a format string and arguments to a 'LT.Text'.
format :: Params ps => Format -> ps -> LT.Text
format fmt ps = toLazyText $ build fmt ps
{-# INLINE format #-}

-- | Render a floating point number, with the given number of digits
-- of precision.  Uses decimal notation for values between @0.1@ and
-- @9,999,999@, and scientific notation otherwise.
prec :: (Real a) =>
        Int
     -- ^ Number of digits of precision.
     -> a -> Builder
{-# RULES "prec/Double"
    forall d x. prec d (x::Double) = B.build (C.toPrecision d x) #-}
prec digits = B.build . C.toPrecision digits . realToFrac
{-# NOINLINE[0] prec #-}

-- | Render a floating point number using scientific/engineering
-- notation (e.g. @2.3e123@), with the given number of decimal places.
expt :: (Real a) =>
        Int
     -- ^ Number of digits of precision after the decimal.
     -> a -> Builder
expt decs = B.build . C.toExponential decs . realToFrac
{-# RULES "expt/Double"
    forall d x. expt d (x::Double) = B.build (C.toExponential d x) #-}
{-# NOINLINE[0] expt #-}

-- | The class of types that can be used as a collection of arguments
-- for formatting.
class Params ps where
    buildParams :: ps -> [Builder]

instance Params () where
    buildParams _ = []

instance (Buildable a) => Params (Only a) where
    buildParams (Only a) = [B.build a]

instance (Buildable a) => Params [a] where
    buildParams = map B.build

instance (Buildable a, Buildable b) => Params (a,b) where
    buildParams (a,b) = [B.build a, B.build b]

instance (Buildable a, Buildable b, Buildable c) => Params (a,b,c) where
    buildParams (a,b,c) = [B.build a, B.build b, B.build c]

instance (Buildable a, Buildable b, Buildable c, Buildable d)
    => Params (a,b,c,d) where
    buildParams (a,b,c,d) =
        [B.build a, B.build b, B.build c, B.build d]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e)
    => Params (a,b,c,d,e) where
    buildParams (a,b,c,d,e) =
        [B.build a, B.build b, B.build c, B.build d, B.build e]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f)
    => Params (a,b,c,d,e,f) where
    buildParams (a,b,c,d,e,f) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g)
    => Params (a,b,c,d,e,f,g) where
    buildParams (a,b,c,d,e,f,g) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h)
    => Params (a,b,c,d,e,f,g,h) where
    buildParams (a,b,c,d,e,f,g,h) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i)
    => Params (a,b,c,d,e,f,g,h,i) where
    buildParams (a,b,c,d,e,f,g,h,i) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j)
    => Params (a,b,c,d,e,f,g,h,i,j) where
    buildParams (a,b,c,d,e,f,g,h,i,j) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k)
    => Params (a,b,c,d,e,f,g,h,i,j,k) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j,
         B.build k]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j,
         B.build k, B.build l]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j,
         B.build k, B.build l, B.build m]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j,
         B.build k, B.build l, B.build m, B.build n]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j,
         B.build k, B.build l, B.build m, B.build n, B.build o]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o,
          Buildable p)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j,
         B.build k, B.build l, B.build m, B.build n, B.build o,
         B.build p]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o,
          Buildable p, Buildable r)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j,
         B.build k, B.build l, B.build m, B.build n, B.build o,
         B.build p, B.build r]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o,
          Buildable p, Buildable r, Buildable s)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j,
         B.build k, B.build l, B.build m, B.build n, B.build o,
         B.build p, B.build r, B.build s]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o,
          Buildable p, Buildable r, Buildable s, Buildable t)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j,
         B.build k, B.build l, B.build m, B.build n, B.build o,
         B.build p, B.build r, B.build s, B.build t]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o,
          Buildable p, Buildable r, Buildable s, Buildable t, Buildable u)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u) =
        [B.build a, B.build b, B.build c, B.build d, B.build e,
         B.build f, B.build g, B.build h, B.build i, B.build j,
         B.build k, B.build l, B.build m, B.build n, B.build o,
         B.build p, B.build r, B.build s, B.build t, B.build u]
