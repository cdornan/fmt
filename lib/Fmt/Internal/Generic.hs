{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Fmt.Internal.Generic where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Sequence (Seq)
#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty (NonEmpty)
#endif

import Data.List
import Data.Text.Lazy.Builder hiding (fromString)
import GHC.Generics
import Formatting.Buildable

import Fmt.Internal.Formatters
import Fmt.Internal.Template
import Fmt.Internal.Tuple


-- $setup
-- >>> import Fmt

{- | Format an arbitrary value without requiring a 'Buildable' instance:

>>> data Foo = Foo { x :: Bool, y :: [Int] } deriving Generic

>>> fmt (genericF (Foo True [1,2,3]))
Foo:
  x: True
  y: [1, 2, 3]

It works for non-record constructors too:

>>> data Bar = Bar Bool [Int] deriving Generic

>>> fmtLn (genericF (Bar True [1,2,3]))
<Bar: True, [1, 2, 3]>

Any fields inside the type must either be 'Buildable' or one of the following
types:

* a function
* a tuple (up to 8-tuples)
* list, 'NonEmpty', 'Seq'
* 'Map', 'IntMap', 'Set', 'IntSet'
* 'Maybe', 'Either'

The exact format of 'genericF' might change in future versions, so don't rely
on it. It's merely a convenience function.
-}
genericF :: (Generic a, GBuildable (Rep a)) => a -> Builder
genericF = gbuild . from

----------------------------------------------------------------------------
-- GBuildable
----------------------------------------------------------------------------

class GBuildable f where
  gbuild :: f a -> Builder

instance Buildable' c => GBuildable (K1 i c) where
  gbuild (K1 a) = build' a

instance (GBuildable a, GBuildable b) => GBuildable (a :+: b) where
  gbuild (L1 x) = gbuild x
  gbuild (R1 x) = gbuild x

instance GBuildable a => GBuildable (M1 D d a) where
  gbuild (M1 x) = gbuild x

instance (GetFields a, Constructor c) => GBuildable (M1 C c a) where
  -- A note on fixity:
  --   * Ordinarily e.g. "Foo" is prefix and e.g. ":|" is infix
  --   * However, "Foo" can be infix when defined as "a `Foo` b"
  --   * And ":|" can be prefix when defined as "(:|) a b"
  gbuild c@(M1 x) = case conFixity c of
    Infix _ _
      | [a, b] <- fields -> format "({} {} {})" a infixName b
      -- this case should never happen, but still
      | otherwise        -> format "<{}: {}>"
                              prefixName
                              (mconcat (intersperse ", " fields))
    Prefix
      | isTuple -> tupleF fields
      | conIsRecord c -> nameF (build prefixName) (blockMapF fieldsWithNames)
      | null (getFields x) -> build prefixName
      -- I believe that there will be only one field in this case
      | null (conName c) -> mconcat (intersperse ", " fields)
      | otherwise -> format "<{}: {}>"
                       prefixName
                       (mconcat (intersperse ", " fields))
    where
      (prefixName, infixName)
        | ":" `isPrefixOf` conName c = ("(" ++ conName c ++ ")", conName c)
        | otherwise                  = (conName c, "`" ++ conName c ++ "`")
      fields          = map snd (getFields x)
      fieldsWithNames = getFields x
      isTuple         = "(," `isPrefixOf` prefixName

----------------------------------------------------------------------------
-- Buildable'
----------------------------------------------------------------------------

-- | A more powerful 'Buildable' used for 'genericF'. Can build functions,
-- tuples, lists, maps, etc., as well as combinations thereof.
class Buildable' a where
  build' :: a -> Builder

instance Buildable' () where
  build' _ = "()"

instance (Buildable' a1, Buildable' a2)
  => Buildable' (a1, a2) where
  build' (a1, a2) = tupleF
    [build' a1, build' a2]

instance (Buildable' a1, Buildable' a2, Buildable' a3)
  => Buildable' (a1, a2, a3) where
  build' (a1, a2, a3) = tupleF
    [build' a1, build' a2, build' a3]

instance (Buildable' a1, Buildable' a2, Buildable' a3, Buildable' a4)
  => Buildable' (a1, a2, a3, a4) where
  build' (a1, a2, a3, a4) = tupleF
    [build' a1, build' a2, build' a3, build' a4]

instance (Buildable' a1, Buildable' a2, Buildable' a3, Buildable' a4,
          Buildable' a5)
  => Buildable' (a1, a2, a3, a4, a5) where
  build' (a1, a2, a3, a4, a5) = tupleF
    [build' a1, build' a2, build' a3, build' a4,
     build' a5]

instance (Buildable' a1, Buildable' a2, Buildable' a3, Buildable' a4,
          Buildable' a5, Buildable' a6)
  => Buildable' (a1, a2, a3, a4, a5, a6) where
  build' (a1, a2, a3, a4, a5, a6) = tupleF
    [build' a1, build' a2, build' a3, build' a4,
     build' a5, build' a6]

instance (Buildable' a1, Buildable' a2, Buildable' a3, Buildable' a4,
          Buildable' a5, Buildable' a6, Buildable' a7)
  => Buildable' (a1, a2, a3, a4, a5, a6, a7) where
  build' (a1, a2, a3, a4, a5, a6, a7) = tupleF
    [build' a1, build' a2, build' a3, build' a4,
     build' a5, build' a6, build' a7]

instance (Buildable' a1, Buildable' a2, Buildable' a3, Buildable' a4,
          Buildable' a5, Buildable' a6, Buildable' a7, Buildable' a8)
  => Buildable' (a1, a2, a3, a4, a5, a6, a7, a8) where
  build' (a1, a2, a3, a4, a5, a6, a7, a8) = tupleF
    [build' a1, build' a2, build' a3, build' a4,
     build' a5, build' a6, build' a7, build' a8]

instance {-# OVERLAPPING #-} Buildable' [Char] where
  build' = build

instance Buildable' a => Buildable' [a] where
  build' = listF' build'

#if MIN_VERSION_base(4,9,0)
instance Buildable' a => Buildable' (NonEmpty a) where
  build' = listF' build'
#endif

instance Buildable' a => Buildable' (Seq a) where
  build' = listF' build'

instance (Buildable' k, Buildable' v) => Buildable' (Map k v) where
  build' = mapF' build' build' . Map.toList

instance (Buildable' v) => Buildable' (Set v) where
  build' = listF' build'

instance (Buildable' v) => Buildable' (IntMap v) where
  build' = mapF' build' build' . IntMap.toList

instance Buildable' IntSet where
  build' = listF' build' . IntSet.toList

instance (Buildable' a) => Buildable' (Maybe a) where
  build' Nothing  = maybeF (Nothing         :: Maybe Builder)
  build' (Just a) = maybeF (Just (build' a) :: Maybe Builder)

instance (Buildable' a, Buildable' b) => Buildable' (Either a b) where
  build' (Left  a) = eitherF (Left  (build' a) :: Either Builder Builder)
  build' (Right a) = eitherF (Right (build' a) :: Either Builder Builder)

instance Buildable' (a -> b) where
  build' _ = "<function>"

instance {-# OVERLAPPABLE #-} Buildable a => Buildable' a where
  build' = build

----------------------------------------------------------------------------
-- GetFields
----------------------------------------------------------------------------

class GetFields f where
  -- | Get fields, together with their names if available
  getFields :: f a -> [(String, Builder)]

instance (GetFields a, GetFields b) => GetFields (a :*: b) where
  getFields (a :*: b) = getFields a ++ getFields b

instance (GBuildable a, Selector c) => GetFields (M1 S c a) where
  getFields s@(M1 a) = [(selName s, gbuild a)]

instance GBuildable a => GetFields (M1 D c a) where
  getFields (M1 a) = [("", gbuild a)]

instance GBuildable a => GetFields (M1 C c a) where
  getFields (M1 a) = [("", gbuild a)]

instance GetFields U1 where
  getFields _ = []
