{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

#include "overlap.h"

{- | A module providing access to internals (in case you really need them).
Can change at any time, though probably won't.

It also provides some functions that are used in 'Fmt.Time' (so that
'Fmt.Time' wouldn't need to import 'Fmt').
-}
module Fmt.Internal
(
  -- * Classes
  FormatAsHex(..),
  FormatAsBase64(..),

  -- * Classes used for 'genericF'
  GBuildable(..),
  GetFields(..),
  Buildable'(..),

  -- * Helpers
  indentF',

  -- * Reexports
  module Fmt.Internal.Core,
  module Fmt.Internal.Format,
  module Fmt.Internal.Tuple,
  module Fmt.Internal.Numeric,
)
where

-- Generic useful things
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif
-- Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
-- 'Buildable' and raw 'Builder' formatters
import qualified Formatting.Internal.Raw as F
-- Text 'Builder'
import           Data.Text.Lazy.Builder hiding (fromString)
-- Bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- Formatting bytestrings
import qualified Data.ByteString.Base16          as B16
import qualified Data.ByteString.Base16.Lazy     as B16L
import qualified Data.ByteString.Base64          as B64
import qualified Data.ByteString.Base64.Lazy     as B64L
import qualified Data.ByteString.Base64.URL      as B64U
import qualified Data.ByteString.Base64.URL.Lazy as B64UL

import Fmt.Internal.Core
import Fmt.Internal.Format
import Fmt.Internal.Tuple
import Fmt.Internal.Numeric

-- $setup
-- >>> import Fmt

----------------------------------------------------------------------------
-- Hex
----------------------------------------------------------------------------

class FormatAsHex a where
  {- |
Format a number or bytestring as hex:

>>> hexF 3635
"e33"
>>> hexF ("\0\50\63\80" :: BS.ByteString)
"00323f50"
  -}
  hexF :: a -> Builder

instance FormatAsHex BS.ByteString where
  hexF = fromText . T.decodeLatin1 . B16.encode

instance FormatAsHex BSL.ByteString where
  hexF = fromLazyText . TL.decodeLatin1 . B16L.encode

instance _OVERLAPPABLE_ Integral a => FormatAsHex a where
  hexF = F.hex

----------------------------------------------------------------------------
-- Base64
----------------------------------------------------------------------------

class FormatAsBase64 a where
  {- |
Convert a bytestring to base64:

>>> base64F ("\0\50\63\80" :: BS.ByteString)
"ADI/UA=="
  -}
  base64F :: a -> Builder
  {- |
Convert a bytestring to base64url (a variant of base64 which omits @\/@ and
thus can be used in URLs):

>>> base64UrlF ("\0\50\63\80" :: BS.ByteString)
"ADI_UA=="
  -}
  base64UrlF :: a -> Builder

instance FormatAsBase64 BS.ByteString where
  base64F    = fromText . T.decodeLatin1 . B64.encode
  base64UrlF = fromText . T.decodeLatin1 . B64U.encode

instance FormatAsBase64 BSL.ByteString where
  base64F    = fromLazyText . TL.decodeLatin1 . B64L.encode
  base64UrlF = fromLazyText . TL.decodeLatin1 . B64UL.encode

----------------------------------------------------------------------------
-- Classes used for 'genericF'
----------------------------------------------------------------------------

class GBuildable f where
  gbuild :: f a -> Builder

class GetFields f where
  -- | Get fields, together with their names if available
  getFields :: f a -> [(String, Builder)]

-- | A more powerful 'Buildable' used for 'genericF'. Can build functions,
-- tuples, lists, maps, etc., as well as combinations thereof.
class Buildable' a where
  build' :: a -> Builder

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

{- | Add a prefix to the first line, and indent all lines but the first one.

The output will always end with a newline, even when the input doesn't.
-}
indentF' :: Int -> T.Text -> Builder -> Builder
indentF' n pref a = case TL.lines (toLazyText a) of
  []     -> fromText pref <> "\n"
  (x:xs) -> fromLazyText $
            TL.unlines $ (TL.fromStrict pref <> x) : map (spaces <>) xs
  where
    spaces = TL.replicate (fromIntegral n) (TL.singleton ' ')
