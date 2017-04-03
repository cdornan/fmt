{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

-- | Benchmarks for @fmt@ library.

module Main where

import           Control.DeepSeq         (NFData)
import           Data.Monoid             ((<>))
import           Data.String             (IsString)
import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Format        as TF
import           Data.Text.Format.Params (Params)
import qualified Data.Text.Lazy          as LT
import           Fmt                     (( #| ), (|#))
import           Formatting              (int, sformat, (%))
import           Text.Printf             (printf)

import           Criterion               (Benchmarkable, bench, bgroup, nf)
import           Criterion.Main          (defaultMain)

simpleBench :: (NFData s, IsString s) => ((Int, Int) -> s) -> Benchmarkable
simpleBench fmt = nf fmt (1,2)

format' :: Params ps => TF.Format -> ps -> Text
format' f = LT.toStrict . TF.format f

main :: IO ()
main = defaultMain
  [ bgroup "simple"
    [ bgroup "text"
      [ bench "fmt" $ simpleBench @Text (\(a,b) -> "hello "#|a|#" world "#|b|#"")
      , bench "fng" $ simpleBench @Text (\(a,b) -> sformat ("hello "%int%" world "%int) a b)
      , bench "shw" $ simpleBench @Text (\(a,b) -> "hello " <> T.pack (show a) <> " world " <> T.pack (show b))
      , bench "tft" $ simpleBench @Text (format' "hello {} world {} ")
      , bench "prf" $ simpleBench @Text (\(a,b) -> T.pack $ printf "hello %d world %d" a b)
      , bench "int" $ simpleBench @Text (\(a,b) -> T.pack [i|hello #{a} world #{b}|])
      ]
    ]
  ]
