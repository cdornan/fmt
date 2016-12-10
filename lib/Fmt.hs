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

  Buildable(..),

  -- * Formatters
  indent,
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

(%<) :: (FromBuilder b) => Builder -> Builder -> b
(%<) str rest = fromBuilder (str <> rest)

(>%) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(>%) a rest = fromBuilder (build a <> rest)

(>%%<) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(>%%<) a rest = fromBuilder (build a <> rest)

infixr 1 %<
infixr 1 >%
infixr 1 >%%<

----------------------------------------------------------------------------
-- Operators with 'Show'
----------------------------------------------------------------------------

(%<<) :: (FromBuilder b) => Builder -> Builder -> b
(%<<) str rest = str %< rest
{-# INLINE (%<<) #-}

(>>%) :: (Show a, FromBuilder b) => a -> Builder -> b
(>>%) a rest = show a >% rest
{-# INLINE (>>%) #-}

(>>%%<<) :: (Show a, FromBuilder b) => a -> Builder -> b
(>>%%<<) a rest = show a >% rest
{-# INLINE (>>%%<<) #-}

infixr 1 %<<
infixr 1 >>%
infixr 1 >>%%<<

----------------------------------------------------------------------------
-- Combinations
----------------------------------------------------------------------------

(>>%%<) :: (Buildable a, FromBuilder b) => a -> Builder -> b
(>>%%<) a rest = a >%%< rest
{-# INLINE (>>%%<) #-}

(>%%<<) :: (Show a, FromBuilder b) => a -> Builder -> b
(>%%<<) a rest = a >>%%<< rest
{-# INLINE (>%%<<) #-}

infixr 1 >>%%<
infixr 1 >%%<<

----------------------------------------------------------------------------
-- Formatters
----------------------------------------------------------------------------

indent :: (FromBuilder b) => Int -> Builder -> b
indent n a = fromBuilder (go (toLazyText a))
  where
    spaces = fromText (T.replicate n (T.singleton ' '))
    -- We don't use 'lines' because it doesn't distinguish between trailing
    -- newline being present/absent. We want the following behavior:
    --     >>> indent 2 "hi"
    --     "  hi"
    --     >>> indent 2 "hi\n"
    --     "  hi\n"
    go t | TL.null t = mempty
    go t = let (l, t') = TL.break ((==) '\n') t
           in spaces <> if TL.null t'
                          then fromLazyText l
                          else fromLazyText l <>
                               (singleton '\n' <> go (TL.tail t'))

-- TODO: something to format a record nicely (with generics, probably)
-- TODO: something like https://hackage.haskell.org/package/groom
-- TODO: write docs
-- TODO: mention printf in description so that it would be findable
-- TODO: mention things that work (<n+1>, <f n>, <show n>)
-- TODO: colors?
-- TODO: add NL for newline?
-- TODO: have to decide on whether it would be >%< or >%%< or maybe >|<
-- TODO: actually, what about |< and >|?
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
