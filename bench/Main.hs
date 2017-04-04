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

import           Criterion               (Benchmarkable, bench, bgroup, nf)
import           Criterion.Main          (defaultMain)

simpleBench :: ((Int, Int) -> Text) -> Benchmarkable
simpleBench fmt = nf fmt (1,2)

format' :: TF.Params ps => TF.Format -> ps -> Text
format' f = LT.toStrict . TF.format f

tshow :: Show a => a -> Text
tshow x = T.pack (show x)

main :: IO ()
main = defaultMain
  [ bgroup "simple"
    [ bgroup "text"
      [ bench "fmt" $ simpleBench
            (\(a,b) -> "hello "#|a|#" world "#|b|#"")
      , bench "formatting" $ simpleBench
            (\(a,b) -> sformat ("hello "%int%" world "%int) a b)
      , bench "show" $ simpleBench
            (\(a,b) -> "hello " <> tshow a <> " world " <> tshow b)
      , bench "text-format" $ simpleBench
            (format' "hello {} world {}")
      , bench "printf" $ simpleBench
            (\(a,b) -> T.pack $ printf "hello %d world %d" a b)
      , bench "interpolate" $ simpleBench
            (\(a,b) -> T.pack [i|hello #{a} world #{b}|])
      ]
    ]
  ]
