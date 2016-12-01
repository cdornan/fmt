{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Fmt.IO
(
  module Fmt,
)
where


import Fmt
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.IO as TL


instance (a ~ ()) => FromBuilder (IO a) where
  fromBuilder = TL.putStr . TL.toLazyText
  {-# INLINE fromBuilder #-}
