{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Benchmarks for @fmt@ library.

module Main where

import           Control.DeepSeq         (NFData)
import           Data.Monoid             ((<>))
import           Data.String             (IsString)
import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Format        as TF
import           Data.Text.Format.Params as TF
import qualified Data.Text.Lazy          as LT
import           Fmt                     (( #| ), (|#))
import           Formatting              (formatToString, int, sformat, (%))
import           Text.Printf             (printf)

import           Criterion               (Benchmark, bench, bgroup, nf)
import           Criterion.Main          (defaultMain)

----------------------------------------------------------------------------
-- Format utility functions
----------------------------------------------------------------------------

format' :: TF.Params ps => TF.Format -> ps -> Text
format' f = LT.toStrict . TF.format f

formatS :: TF.Params ps => TF.Format -> ps -> String
formatS f = LT.unpack . TF.format f

tshow :: Show a => a -> Text
tshow x = T.pack (show x)

----------------------------------------------------------------------------
-- Benchmarks utility functions
----------------------------------------------------------------------------

bGenericStringGroup :: (NFData s, IsString s) => String -> a -> [(String, a -> s)] -> Benchmark
bGenericStringGroup sTag benchObj = bgroup sTag . map (\(tag, fmt) -> bench tag (nf fmt benchObj))
{-# INLINABLE bGenericStringGroup #-}

bTextGroup :: a -> [(String, a -> Text)] -> Benchmark
bTextGroup = bGenericStringGroup "text"
{-# INLINE bTextGroup #-}

bStringGroup :: a -> [(String, a -> String)] -> Benchmark
bStringGroup = bGenericStringGroup "string"
{-# INLINE bStringGroup #-}

-- Function for convenience instead of using manual @(,)@.
taggedB :: String -> (a -> b) -> (String, a -> b)
taggedB = (,)

----------------------------------------------------------------------------
-- Benchmakrs themselves
----------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "simple"
    [ bTextGroup (1 :: Int, 2 :: Int)
      [ taggedB "fmt"         $ \(a,b) -> "hello "#|a|#" world "#|b|#""
      , taggedB "formattting" $ \(a,b) -> sformat ("hello "%int%" world "%int) a b
      , taggedB "text-format" $ format' "hello {} world {}"
      , taggedB "interpolate" $ \(a,b) -> T.pack [i|hello #{a} world #{b}|]
      , taggedB "show"        $ \(a,b) -> "hello " <> tshow a <> " world " <> tshow b
      , taggedB "printf"      $ \(a,b) -> T.pack $ printf "hello %d world %d" a b
      ]
    , bStringGroup (1 :: Int, 2 :: Int)
      [ taggedB "fmt"         $ \(a,b) -> "hello "#|a|#" world "#|b|#""
      , taggedB "formattting" $ \(a,b) -> formatToString ("hello "%int%" world "%int) a b
      , taggedB "text-format" $ formatS "hello {} world {}"
      , taggedB "interpolate" $ \(a,b) -> [i|hello #{a} world #{b}|]
      , taggedB "show"        $ \(a,b) -> "hello " ++ show a ++ " world " ++ show b
      , taggedB "printf"      $ \(a,b) -> printf "hello %d world %d" a b
      ]
    ]
  ]
