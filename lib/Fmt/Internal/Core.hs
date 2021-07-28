{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}


module Fmt.Internal.Core where

#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Text.Lazy.Builder hiding (fromString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BB
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

instance FromBuilder BS.ByteString where
  fromBuilder = T.encodeUtf8 . TL.toStrict . toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder BSL.ByteString where
  fromBuilder = TL.encodeUtf8 . toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder BB.Builder where
  fromBuilder = TL.encodeUtf8Builder . toLazyText
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

(|++||) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(|++||) a rest = a |++| rest
{-# INLINE (|++||) #-}

(||++|) :: (Show a, FromBuilder b) => a -> Builder -> b
(||++|) a rest = a ||++|| rest
{-# INLINE (||++|) #-}

infixr 1 |++|
infixr 1 ||++||
infixr 1 ||++|
infixr 1 |++||

----------------------------------------------------------------------------
-- Functions
----------------------------------------------------------------------------

{- | 'fmt' converts things to 'String', 'T.Text', 'BS.ByteString' or 'Builder'.

Most of the time you won't need it, as strings produced with ('+|') and
('|+') can already be used as 'String', 'T.Text', etc. However, combinators
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

{- | 'pretty' shows a value using its 'Buildable' instance.
-}
pretty :: (Buildable a, FromBuilder b) => a -> b
pretty = fmt . build
{-# INLINE pretty #-}

{- | Like 'pretty', but appends a newline.
-}
prettyLn :: (Buildable a, FromBuilder b) => a -> b
prettyLn = fmtLn . build
{-# INLINE prettyLn #-}
