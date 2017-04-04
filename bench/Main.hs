{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Benchmarks for @fmt@ library.

module Main where

import           Data.Monoid             ((<>))
import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Format        as TF
import           Data.Text.Format.Params as TF
import qualified Data.Text.Lazy          as LT
import           Fmt                     (( #| ), (|#))
import           Formatting              (int, sformat, (%))
import           Text.Printf             (printf)

import           Criterion               (Benchmark, Benchmarkable, bench, bgroup, nf)
import           Criterion.Main          (defaultMain)

----------------------------------------------------------------------------
-- Format utility functions
----------------------------------------------------------------------------

format' :: TF.Params ps => TF.Format -> ps -> Text
format' f = LT.toStrict . TF.format f

tshow :: Show a => a -> Text
tshow x = T.pack (show x)

----------------------------------------------------------------------------
-- Benchmarks utility functions
----------------------------------------------------------------------------

simpleBench :: ((Int, Int) -> Text) -> Benchmarkable
simpleBench fmt = nf fmt (1,2)

bTextGroup :: a -> [(String, a -> Text)] -> Benchmark
bTextGroup benchObj = bgroup "text" . map (\(tag, fmt) -> bench tag (nf fmt benchObj))

-- Function for convenience instead of using manual @(,)@.
taggedB :: String -> (a -> b) -> (String, a -> b)
taggedB = (,)

----------------------------------------------------------------------------
-- Benchmakrs themselves
----------------------------------------------------------------------------
{-
bTextGroup
[ ("fmt", \(a,b) -> "")
]
-}
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
    ]
  ]
