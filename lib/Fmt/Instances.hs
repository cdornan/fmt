{-# LANGUAGE OverloadedStrings #-}

-- | Module containin orphan 'Buildable' instances
-- for different data types.

module Fmt.Instances () where

import           Data.Text.Buildable (Buildable (..))

-- TODO: reexport time-units itself?
import           Data.Time.Units     (Attosecond, Day, Femtosecond, Fortnight, Hour,
                                      Microsecond, Millisecond, Minute, Nanosecond,
                                      Picosecond, Second, Week)

----------------------------------------------------------------------------
-- time-units instances
----------------------------------------------------------------------------

instance Buildable Attosecond  where build = build . show
instance Buildable Femtosecond where build = build . show
instance Buildable Picosecond  where build = build . show
instance Buildable Nanosecond  where build = build . show
instance Buildable Microsecond where build = build . show
instance Buildable Millisecond where build = build . show
instance Buildable Second      where build = build . show
instance Buildable Minute      where build = build . show
instance Buildable Hour        where build = build . show
instance Buildable Day         where build = build . show
instance Buildable Week        where build = build . show
instance Buildable Fortnight   where build = build . show
