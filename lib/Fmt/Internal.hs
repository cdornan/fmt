{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

#include "overlap.h"

{- | A module providing access to internals (in case you really need them).
Can change at any time, though probably won't.
-}
module Fmt.Internal
(
  -- * Classes
  FormatAsHex(..),
  FormatAsBase64(..),

  -- * Reexports
  module Fmt.Internal.Core,
  module Fmt.Internal.Formatters,
  module Fmt.Internal.Template,
  module Fmt.Internal.Tuple,
  module Fmt.Internal.Numeric,
  module Fmt.Internal.Generic,
)
where

-- Generic useful things
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif
-- Text
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
-- 'Buildable' and raw 'Builder' formatters
import qualified Formatting.Internal.Raw as F
-- Text 'Builder'
import           Data.Text.Lazy.Builder hiding (fromString)
-- Bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- Formatting bytestrings
import qualified Data.ByteString.Builder         as BB
import qualified Data.ByteString.Base64          as B64
import qualified Data.ByteString.Base64.Lazy     as B64L
import qualified Data.ByteString.Base64.URL      as B64U
import qualified Data.ByteString.Base64.URL.Lazy as B64UL

import Fmt.Internal.Core
import Fmt.Internal.Formatters
import Fmt.Internal.Template
import Fmt.Internal.Tuple
import Fmt.Internal.Numeric
import Fmt.Internal.Generic

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
  hexF = fromLazyText . TL.decodeLatin1 . BB.toLazyByteString . BB.byteStringHex

instance FormatAsHex BSL.ByteString where
  hexF = fromLazyText . TL.decodeLatin1 . BB.toLazyByteString . BB.lazyByteStringHex

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
