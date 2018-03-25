{-# LANGUAGE OverloadedStrings #-}


module Fmt.Internal.Tuple
(
  TupleF(..),
  renderTuple,
)
where


import           Data.List (intersperse)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder
import           Formatting.Buildable (Buildable, build)
import           Lens.Micro


-- $setup
-- >>> import Fmt

class TupleF a where
  {- |
Format a tuple (of up to 8 elements):

>>> tupleF (1,2,"hi")
"(1, 2, hi)"

If any of the elements takes several lines, an alternate format is used:

>>> fmt $ tupleF ("test","foo\nbar","more test")
( test
,
  foo
  bar
,
  more test )

You can also use 'tupleF' on lists to get tuple-like formatting.
  -}
  tupleF :: a -> Builder

instance (Buildable a1, Buildable a2)
  => TupleF (a1, a2) where
  tupleF (a1, a2) = renderTuple
    [build a1, build a2]

instance (Buildable a1, Buildable a2, Buildable a3)
  => TupleF (a1, a2, a3) where
  tupleF (a1, a2, a3) = renderTuple
    [build a1, build a2, build a3]

instance (Buildable a1, Buildable a2, Buildable a3, Buildable a4)
  => TupleF (a1, a2, a3, a4) where
  tupleF (a1, a2, a3, a4) = renderTuple
    [build a1, build a2, build a3, build a4]

instance (Buildable a1, Buildable a2, Buildable a3, Buildable a4,
          Buildable a5)
  => TupleF (a1, a2, a3, a4, a5) where
  tupleF (a1, a2, a3, a4, a5) = renderTuple
    [build a1, build a2, build a3, build a4,
     build a5]

instance (Buildable a1, Buildable a2, Buildable a3, Buildable a4,
          Buildable a5, Buildable a6)
  => TupleF (a1, a2, a3, a4, a5, a6) where
  tupleF (a1, a2, a3, a4, a5, a6) = renderTuple
    [build a1, build a2, build a3, build a4,
     build a5, build a6]

instance (Buildable a1, Buildable a2, Buildable a3, Buildable a4,
          Buildable a5, Buildable a6, Buildable a7)
  => TupleF (a1, a2, a3, a4, a5, a6, a7) where
  tupleF (a1, a2, a3, a4, a5, a6, a7) = renderTuple
    [build a1, build a2, build a3, build a4,
     build a5, build a6, build a7]

instance (Buildable a1, Buildable a2, Buildable a3, Buildable a4,
          Buildable a5, Buildable a6, Buildable a7, Buildable a8)
  => TupleF (a1, a2, a3, a4, a5, a6, a7, a8) where
  tupleF (a1, a2, a3, a4, a5, a6, a7, a8) = renderTuple
    [build a1, build a2, build a3, build a4,
     build a5, build a6, build a7, build a8]

instance Buildable a => TupleF [a] where
  tupleF = renderTuple . map build

{- |
Format a list like a tuple. (This function is used to define 'tupleF'.)
-}
renderTuple :: [Builder] -> Builder
renderTuple xs
  | True `elem` mls = mconcat (intersperse ",\n" items)
  | otherwise = "(" <> mconcat (intersperse ", " xs) <> ")"
  where
    (mls, items) = unzip $ zipWith3 buildItem
                             xs (set _head True falses) (set _last True falses)
    -- A list of 'False's which has the same length as 'xs'
    falses = map (const False) xs
    -- Returns 'True' if the item is multiline
    buildItem :: Builder
              -> Bool              -- ^ Is the item the first?
              -> Bool              -- ^ Is the item the last?
              -> (Bool, Builder)
    buildItem x isFirst isLast =
      case map fromLazyText (TL.lines (toLazyText x)) of
        [] | isFirst && isLast -> (False, "()\n")
           | isFirst           -> (False, "(\n")
           |            isLast -> (False, "  )\n")
           | otherwise         -> (False, "")
        ls ->
           (not (null (tail ls)),
            mconcat . map (<> "\n") $
              ls & _head %~ (if isFirst then ("( " <>) else ("  " <>))
                 & _tail.each %~ ("  " <>)
                 & _last %~ (if isLast then (<> " )") else id))
