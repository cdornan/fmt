{-# LANGUAGE FlexibleInstances #-}

module Fmt
(
  (%<),
  (>%),

  FromBuilder(..),
)
where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder hiding (fromString)
import Data.Monoid
import Data.Text.Buildable

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

(%<) :: (Buildable a, FromBuilder b) => Builder -> a -> b
(%<) x a = fromBuilder (x <> build a)

(>%) :: (FromBuilder b) => Builder -> Builder -> b
(>%) x a = fromBuilder (x <> a)
