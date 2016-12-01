{-# LANGUAGE FlexibleInstances #-}

module Fmt
(
  (%<),
  (>%),
  (>%%<),

  (%<<),
  (>>%),
  (>>%%<<),

  (>%%<<),
  (>>%%<),

  FromBuilder(..),
)
where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder hiding (fromString)
import Data.Monoid
import Data.Text.Buildable

----------------------------------------------------------------------------
-- Operators with 'Buildable'
----------------------------------------------------------------------------

(%<) :: (Buildable a, FromBuilder b) => Builder -> a -> b
(%<) x a = fromBuilder (x <> build a)

(>%) :: (FromBuilder b) => Builder -> Builder -> b
(>%) x a = fromBuilder (x <> a)

(>%%<) :: (Buildable a, FromBuilder b) => Builder -> a -> b
(>%%<) x a = fromBuilder (x <> build a)

----------------------------------------------------------------------------
-- Operators with 'Show'
----------------------------------------------------------------------------

(%<<) :: (Show a, FromBuilder b) => Builder -> a -> b
(%<<) x a = x %< show a
{-# INLINE (%<<) #-}

(>>%) :: (FromBuilder b) => Builder -> Builder -> b
(>>%) x a = x >% a
{-# INLINE (>>%) #-}

(>>%%<<) :: (Show a, FromBuilder b) => Builder -> a -> b
(>>%%<<) x a = x %< show a
{-# INLINE (>>%%<<) #-}

----------------------------------------------------------------------------
-- Combinations
----------------------------------------------------------------------------

(>>%%<) :: (Buildable a, FromBuilder b) => Builder -> a -> b
(>>%%<) x a = x >%%< a
{-# INLINE (>>%%<) #-}

(>%%<<) :: (Show a, FromBuilder b) => Builder -> a -> b
(>%%<<) x a = x >>%%<< a
{-# INLINE (>%%<<) #-}

-- TODO: an IO () instance? so that it would work as a cool printf-less printf
-- TODO: something for indentation
-- TODO: something to format a record nicely (with generics, probably)
-- TODO: something like https://hackage.haskell.org/package/groom
-- TODO: reexport Buildable
-- TODO: write docs
-- TODO: colors?
-- TODO: tests
-- TODO: what effect does it have on compilation time? what effect do
--       other formatting libraries have on compilation time?

class FromBuilder a where
  fromBuilder :: Builder -> a

instance FromBuilder Builder where
  fromBuilder = id
  {-# INLINE fromBuilder #-}

instance FromBuilder String where
  fromBuilder = TL.unpack . toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder T.Text where
  fromBuilder = TL.toStrict . toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder TL.Text where
  fromBuilder = toLazyText
  {-# INLINE fromBuilder #-}
