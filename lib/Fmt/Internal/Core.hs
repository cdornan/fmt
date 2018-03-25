{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}


module Fmt.Internal.Core
(
  -- * Class
  FromBuilder(..),

  -- * Operators
  -- ** Ordinary
  (+|), (|+),
  -- ** 'Show'
  (+||), (||+),
  -- ** Combinations
  (|++|), (||++||), (||++|), (|++||),

  -- * Functions
  fmt, fmtLn,
)
where


import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import           Data.Text.Lazy.Builder hiding (fromString)
import           Formatting.Buildable (Buildable(..))


----------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------

class FromBuilder a where
  -- | Convert a 'Builder' to something else.
  fromBuilder :: Builder -> a

instance FromBuilder Builder where
  fromBuilder = id
  {-# INLINE fromBuilder #-}

instance (a ~ Char) => FromBuilder [a] where
  fromBuilder = TL.unpack . toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder T.Text where
  fromBuilder = TL.toStrict . toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder TL.Text where
  fromBuilder = toLazyText
  {-# INLINE fromBuilder #-}

instance (a ~ ()) => FromBuilder (IO a) where
  fromBuilder = TL.putStr . toLazyText
  {-# INLINE fromBuilder #-}

----------------------------------------------------------------------------
-- Operators
----------------------------------------------------------------------------

-- | Concatenate, then convert.
(+|) :: (FromBuilder b) => Builder -> Builder -> b
(+|) str rest = fromBuilder (str <> rest)

-- | 'build' and concatenate, then convert.
(|+) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(|+) a rest = fromBuilder (build a <> rest)

infixr 1 +|
infixr 1 |+

-- | Concatenate, then convert.
(+||) :: (FromBuilder b) => Builder -> Builder -> b
(+||) str rest = str +| rest
{-# INLINE (+||) #-}

-- | 'show' and concatenate, then convert.
(||+) :: (Show a, FromBuilder b) => a -> Builder -> b
(||+) a rest = show a |+ rest
{-# INLINE (||+) #-}

infixr 1 +||
infixr 1 ||+

(|++|) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(|++|) a rest = fromBuilder (build a <> rest)
{-# INLINE (|++|) #-}

(||++||) :: (Show a, FromBuilder b) => a -> Builder -> b
(||++||) a rest = show a |+ rest
{-# INLINE (||++||) #-}

(||++|) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(||++|) a rest = a |++| rest
{-# INLINE (||++|) #-}

(|++||) :: (Show a, FromBuilder b) => a -> Builder -> b
(|++||) a rest = a ||++|| rest
{-# INLINE (|++||) #-}

infixr 1 |++|
infixr 1 ||++||
infixr 1 ||++|
infixr 1 |++||

----------------------------------------------------------------------------
-- Functions
----------------------------------------------------------------------------

{- | 'fmt' converts things to 'String', 'Text' or 'Builder'.

Most of the time you won't need it, as strings produced with ('+|') and
('|+') can already be used as 'String', 'Text', etc. However, combinators
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
