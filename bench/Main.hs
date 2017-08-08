{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Benchmarks for @fmt@ library.

module Main where

import           Control.DeepSeq         (NFData)
import           Data.Monoid             ((<>))
import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Format        as TF
import           Data.Text.Format.Params as TF
import qualified Data.Text.Lazy          as LT
import           Fmt                     ((+|), (|+))
import           Formatting              (Format, formatToString, int, sformat, stext,
                                          string, (%))
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

-- Shorter alias for @formatToString@.
fs :: Format String a -> a
fs = formatToString

tshow :: Show a => a -> Text
tshow x = T.pack (show x)

----------------------------------------------------------------------------
-- Benchmarks utility functions
----------------------------------------------------------------------------

bGenericStringGroup :: NFData s => String -> a -> [(String, a -> s)] -> Benchmark
bGenericStringGroup sTag benchObj =
    bgroup sTag . map (\(tag, howToFmt) -> bench tag (nf howToFmt benchObj))
{-# INLINE bGenericStringGroup #-}

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
      [ taggedB "fmt" $
          \(a,b) -> "hello "+|a|+" world "+|b|+""
      , taggedB "formatting" $
          \(a,b) -> sformat ("hello "%int%" world "%int) a b
      , taggedB "text-format" $
          format' "hello {} world {}"
      , taggedB "interpolate" $
          \(a,b) -> T.pack [i|hello #{a} world #{b}|]
      , taggedB "show" $
          \(a,b) -> "hello " <> tshow a <> " world " <> tshow b
      , taggedB "printf" $
          \(a,b) -> T.pack $ printf "hello %d world %d" a b
      ]
    , bStringGroup (1 :: Int, 2 :: Int)
      [ taggedB "fmt" $
          \(a,b) -> "hello "+|a|+" world "+|b|+""
      , taggedB "formatting" $
          \(a,b) -> fs ("hello "%int%" world "%int) a b
      , taggedB "text-format" $
          formatS "hello {} world {}"
      , taggedB "interpolate" $
          \(a,b) -> [i|hello #{a} world #{b}|]
      , taggedB "show" $
          \(a,b) -> "hello " ++ show a ++ " world " ++ show b
      , taggedB "printf" $
          \(a,b) -> printf "hello %d world %d" a b
      ]
    ]

  , bgroup "readme"
    [ bTextGroup (9 :: Int, "Beijing" :: Text)
      [ taggedB "fmt" $
          \(n,city) -> "There are "+|n|+" million bicycles in "+|city|+"."
      , taggedB "formatting" $
          \(n,city) -> sformat ("There are "%int%" million bicycles in "%stext%".") n city
      , taggedB "text-format" $
          format' "There are {} million bicycles in {}."
      , taggedB "interpolate" $
          \(n,city) -> T.pack [i|There are #{n} million bicycles in #{city}.|]
      , taggedB "show" $
          \(n,city) -> "There are " <> tshow n <> " million bicycles in " <> city <> "."
      , taggedB "printf" $
          \(n,city) -> T.pack $ printf "There are %d million bicycles in %s." n city
      ]
    , bStringGroup (9 :: Int, "Beijing" :: String)
      [ taggedB "fmt" $
          \(n,city) -> "There are "+|n|+" million bicycles in "+|city|+"."
      , taggedB "formatting" $
          \(n,city) -> fs ("There are "%int%" million bicycles in "%string%".") n city
      , taggedB "text-format" $
          formatS "There are {} million bicycles in {}."
      , taggedB "interpolate" $
          \(n,city) -> [i|There are #{n} million bicycles in #{city}.|]
      , taggedB "show" $
          \(n,city) -> "There are " ++ show n ++ " million bicycles in " ++ city ++ "."
      , taggedB "printf" $
          \(n,city) -> printf "There are %d million bicycles in %s." n city
      ]
    ]
  ]
