{-# LANGUAGE
OverloadedStrings,
QuasiQuotes
  #-}


module Main where


-- Monoid
import Data.Monoid ((<>))
-- Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import NeatInterpolation
-- Various data structures
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- Tests
import Test.Hspec

-- Fmt
import Fmt


main :: IO ()
main = hspec $ do
  let n = 25 :: Integer
      s = "!" :: String

  it "simple examples" $ do
    ("a"%<n>%"b") ==%> "a25b"
    ("a"%<n>%"b"%<s>%"") ==%> "a25b!"
    (""%<n>%%<s>%"") ==%> "25!"
    (""%<negate n>%%<s>%"") ==%> "-25!"
    (""%<Just n>%%<s>%"") ==%> "25!"

  describe "examples with Show/mixed" $ do
    it "copy of Buildable examples" $ do
      ("a"%<<n>>%"b") ==%> "a25b"
      ("a"%<<n>>%%<<n>>%"b") ==%> "a2525b"
      -- These are mixed, i.e. both Buildable and Show versions are used
      ("a"%<<n>>%"b"%<s>%"") ==%> "a25b!"
      (""%<<n>>%%<s>%"") ==%> "25!"
      (""%<<negate n>>%%<s>%"") ==%> "-25!"
    it "examples that don't work with Buildable" $ do
      (""%<<Just n>>%"") ==%> "Just 25"
      (""%<<(n,n)>>%"") ==%> "(25,25)"

  it "plays nice with other operators" $ do
    -- If precedence is bad these won't compile
    (""%<n-1>%%<n+1>%"") ==%> "2426"
    (id $ ""%<n-1>%%<n+1>%"") ==%> "2426"

  it "works with <>" $ do
    ("number: "%<n>%"\n"<>
     "string: "%<s>%"") ==%> "number: 25\nstring: !"

  describe "output as" $ do
    it "String" $
      ("a"%<n>%"b" :: String) `shouldBe` "a25b"
    it "Text" $
      ("a"%<n>%"b" :: Text) `shouldBe` "a25b"
    it "Lazy Text" $
      ("a"%<n>%"b" :: TL.Text) `shouldBe` "a25b"
    it "Builder" $
      ("a"%<n>%"b" :: Builder) `shouldBe` "a25b"


  describe "formatters" $ do
    describe "'indent'" $ do
      it "simple examples" $ do
        indent 0 "hi" ==%> "hi"
        indent 0 "\nhi\n\n" ==%> "\nhi\n\n"
        indent 2 "hi" ==%> "  hi"
        indent 2 "hi\n" ==%> "  hi\n"
        indent 2 "" ==%> ""
        indent 2 "hi\nbye" ==%> "  hi\n  bye"
        indent 2 "hi\nbye\n" ==%> "  hi\n  bye\n"
      it "formatting a block" $ do
        ("Some numbers:\n"<>
         indent 2 (
           "odd: "%<n>%"\n"<>
           "even: "%<n+1>%"")) ==%> "Some numbers:\n  odd: 25\n  even: 26"

    describe "'listF'" $ do
      it "simple examples" $ do
        listF ([] :: [Int]) ==#> "[]"
        listF [n] ==#> "[25]"
        listF [n,n+1] ==#> "[25, 26]"
        listF [s,s<>s,"",s<>s<>s] ==#> "[!, !!, , !!!]"
      it "different Foldables" $ do
        listF ([1,2,3] :: [Int]) ==#> "[1, 2, 3]"
        listF (V.fromList [1,2,3] :: Vector Int) ==#> "[1, 2, 3]"
        listF (M.fromList [(1,2),(3,4),(5,6)] :: Map Int Int) ==#> "[2, 4, 6]"

    describe "'blockListF'" $ do
      it "empty list" $ do
        blockListF ([] :: [Int]) ==#> "[]\n"
      it "null elements" $ do
        blockListF ([""] :: [Text]) ==#> [text|
          -
          |]
        blockListF (["",""] :: [Text]) ==#> [text|
          -
          -
          |]
        blockListF (["","a",""] :: [Text]) ==#> [text|
          -
          - a
          -
          |]
      it "single-line elements" $ do
        blockListF (["a"] :: [Text]) ==#> [text|
          - a
          |]
        blockListF (["a","b"] :: [Text]) ==#> [text|
          -_a
          -_b
          |]
        blockListF (["a","b","ccc"] :: [Text]) ==#> [text|
          - a
          - b
          - ccc
          |]
      it "multi-line elements" $ do
        blockListF (["a\nx"] :: [Text]) ==#> [text|
          - a
            x
          |]
        blockListF (["a\n x"] :: [Text]) ==#> [text|
          - a
             x
          |]
        blockListF (["a\n x"," b\nxx\ny y ","c\n\n"] :: [Text]) ==#> [text|
          - a
             x

          -  b
            xx
            y y_

          - c
          __
          |]
      it "mix of single-line and multi-line" $ do
        blockListF (["a\nx","b"] :: [Text]) ==#> [text|
          - a
            x

          - b
          |]
        blockListF (["a\nx","b\n"] :: [Text]) ==#> [text|
          - a
            x

          - b
          |]
        blockListF (["a"," b\nxx\ny y ","c\n\n"] :: [Text]) ==#> [text|
          - a

          -  b
            xx
            y y_

          - c
          __
          |]
    describe "'jsonListF'" $ do
      it "empty list" $ do
        jsonListF ([] :: [Int]) ==#> "[]\n"
      it "null elements" $ do
        jsonListF ([""] :: [Text]) ==#> [text|
          [

          ]
          |]
        jsonListF (["",""] :: [Text]) ==#> [text|
          [

          ,
          ]
          |]
        jsonListF (["","a",""] :: [Text]) ==#> [text|
          [

          , a
          ,
          ]
          |]
      it "single-line elements" $ do
        jsonListF (["a"] :: [Text]) ==#> [text|
          [
            a
          ]
          |]
        jsonListF (["a","b"] :: [Text]) ==#> [text|
          [
            a
          , b
          ]
          |]
        jsonListF (["a","b","ccc"] :: [Text]) ==#> [text|
          [
            a
          , b
          , ccc
          ]
          |]
      it "multi-line elements" $ do
        jsonListF (["a\nx"] :: [Text]) ==#> [text|
          [
            a
            x
          ]
          |]
        jsonListF (["a\n x"] :: [Text]) ==#> [text|
          [
            a
             x
          ]
          |]
        jsonListF (["a\n x"," b\nxx\ny y ","c\n\n"] :: [Text]) ==#> [text|
          [
            a
             x
          ,  b
            xx
            y y_
          , c
          __
          ]
          |]
      it "mix of single-line and multi-line" $ do
        jsonListF (["a\nx","b"] :: [Text]) ==#> [text|
          [
            a
            x
          , b
          ]
          |]
        jsonListF (["a\nx","b\n"] :: [Text]) ==#> [text|
          [
            a
            x
          , b
          ]
          |]
        jsonListF (["a"," b\nxx\ny y ","c\n\n"] :: [Text]) ==#> [text|
          [
            a
          ,  b
            xx
            y y_
          , c
          __
          ]
          |]

    describe "'mapF'" $ do
      it "simple examples" $ do
        mapF ([] :: [(Int, Int)]) ==#> "{}"
        mapF [(n,n+1)] ==#> "{25: 26}"
        mapF [(s,n)] ==#> "{!: 25}"
        mapF [('a',True),('b',False),('c',True)] ==#>
          "{a: True, b: False, c: True}"
      it "different map types" $ do
        let m = [('a',True),('b',False),('d',False),('c',True)]
        mapF m ==#> "{a: True, b: False, d: False, c: True}"
        mapF (M.fromList m) ==#> "{a: True, b: False, c: True, d: False}"

    describe "'blockMapF'" $ do
      it "empty map" $ do
        blockMapF ([] :: [(Int, Int)]) ==#> "{}\n"
      it "complex example" $ do
        blockMapF ([("hi", ""),
                    ("foo"," a\n  b"),
                    ("bar","a"),
                    ("baz","a\ng")] :: [(Text, Text)]) ==#> [text|
          hi:
          foo:
             a
              b
          bar: a
          baz:
            a
            g
          |]

    describe "'jsonMapF'" $ do
      it "empty map" $ do
        jsonMapF ([] :: [(Int, Int)]) ==#> "{}\n"
      it "complex example" $ do
        jsonMapF ([("hi", ""),
                   ("foo"," a\n  b"),
                   ("bar","a"),
                   ("baz","a\ng")] :: [(Text, Text)]) ==#> [text|
          {
            hi:
          , foo:
               a
                b
          , bar: a
          , baz:
              a
              g
          }
          |]

    describe "tuples" $ do
      it "tupleF" $ do
        -- we don't need complicated tests here, they're all tested in
        -- 'tupleLikeF' tests
        tupleF (n, s) ==#> "(25, !)"
        tupleF (n, s, n, s) ==#> "(25, !, 25, !)"
        tupleF (n, s, n, s, 'a', 'b', 'c', 'd') ==#>
          "(25, !, 25, !, a, b, c, d)"
      describe "tupleLikeF" $ do
        describe "one-line" $ do
          it "()" $ do
            tupleLikeF [] ==#> "()"
          it "('')" $ do
            tupleLikeF [""] ==#> "()"
          it "(a)" $ do
            tupleLikeF ["a"] ==#> "(a)"
          it "(a,b)" $ do
            tupleLikeF ["a", "b"] ==#> "(a, b)"
          it "(a,'')" $ do
            tupleLikeF ["a", ""] ==#> "(a, )"
          it "('',b)" $ do
            tupleLikeF ["", "b"] ==#> "(, b)"
          it "('','')" $ do
            tupleLikeF ["", ""] ==#> "(, )"
          it "(a,b,c)" $ do
            tupleLikeF ["a", "ba", "caba"] ==#> "(a, ba, caba)"
        it "weird case" $ do
          -- not sure whether I should fix it or not
          tupleLikeF ["a\n"] ==#> "(a\n)"
        describe "multiline" $ do
          describe "all non-empty" $ do
            it "1 element (2 lines)" $ do
              tupleLikeF ["a\nx"] ==#> [text|
                ( a
                  x )
                |]
              tupleLikeF ["a\n x"] ==#> [text|
                ( a
                   x )
                |]
              tupleLikeF [" a\nx\n"] ==#> [text|
                (  a
                  x )
                |]
            it "1 element (3 lines)" $ do
              tupleLikeF ["a\nb\nc"] ==#> [text|
                ( a
                  b
                  c )
                |]
            it "2 elements (1 line + 2 lines)" $ do
              tupleLikeF ["a", "b\nc"] ==#> [text|
                ( a
                ,
                  b
                  c )
                |]
            it "2 elements (2 lines + 1 line)" $ do
              tupleLikeF ["a\nb", "c"] ==#> [text|
                ( a
                  b
                ,
                  c )
                |]
            it "3 elements (each has 2 lines)" $ do
              tupleLikeF ["a\nb", "c\nd", "e\nf"] ==#> [text|
                ( a
                  b
                ,
                  c
                  d
                ,
                  e
                  f )
                |]
          describe "some empty" $ do
            it "2 elements (0 + 2)" $ do
              tupleLikeF ["", "a\nb"] ==#> [text|
                (
                ,
                  a
                  b )
                |]
            it "2 elements (2 + 0)" $ do
              tupleLikeF ["a\nb", ""] ==#> [text|
                ( a
                  b
                ,
                  )
                |]
            it "3 elements (0 + 2 + 0)" $ do
              tupleLikeF ["", "a\nb", ""] ==#> [text|
                (
                ,
                  a
                  b
                ,
                  )
                |]
            it "3 elements (2 + 0 + 2)" $ do
              tupleLikeF ["a\nb", "", "c\nd"] ==#> [text|
                ( a
                  b
                ,
                ,
                  c
                  d )
                |]
            it "4 elements (2 + 0 + 0 + 2)" $ do
              tupleLikeF ["a\nb", "", "", "c\nd"] ==#> [text|
                ( a
                  b
                ,
                ,
                ,
                  c
                  d )
                |]

    describe "ADTs" $ do
      it "maybeF" $ do
        maybeF (Nothing :: Maybe Int) ==#> "<Nothing>"
        maybeF (Just 3 :: Maybe Int) ==#> "3"
      it "eitherF" $ do
        eitherF (Left 1 :: Either Int Int) ==#> "<Left:> 1"
        eitherF (Right 1 :: Either Int Int) ==#> "<Right:> 1"

    describe "padding" $ do
      it "prefixF" $ do
        prefixF (-1) ("hello" :: Text) ==#> ""
        prefixF   0  ("hello" :: Text) ==#> ""
        prefixF   1  ("hello" :: Text) ==#> "h"
        prefixF   2  ("hello" :: Text) ==#> "he"
        prefixF   3  ("hello" :: Text) ==#> "hel"
        prefixF   5  ("hello" :: Text) ==#> "hello"
        prefixF 1000 ("hello" :: Text) ==#> "hello"
        prefixF 1000 (""      :: Text) ==#> ""
      it "suffixF" $ do
        suffixF (-1) ("hello" :: Text) ==#> ""
        suffixF   0  ("hello" :: Text) ==#> ""
        suffixF   1  ("hello" :: Text) ==#> "o"
        suffixF   2  ("hello" :: Text) ==#> "lo"
        suffixF   3  ("hello" :: Text) ==#> "llo"
        suffixF   5  ("hello" :: Text) ==#> "hello"
        suffixF 1000 ("hello" :: Text) ==#> "hello"
        suffixF 1000 (""      :: Text) ==#> ""
      it "padLeftF" $ do
        padLeftF (-1) '!' ("hello" :: Text) ==#> "hello"
        padLeftF   0  '!' ("hello" :: Text) ==#> "hello"
        padLeftF   1  '!' ("hello" :: Text) ==#> "hello"
        padLeftF   5  '!' ("hello" :: Text) ==#> "hello"
        padLeftF   6  '!' ("hello" :: Text) ==#> "!hello"
        padLeftF   7  '!' ("hello" :: Text) ==#> "!!hello"
        padLeftF   7  '!' (""      :: Text) ==#> "!!!!!!!"
      it "padRightF" $ do
        padRightF (-1) '!' ("hello" :: Text) ==#> "hello"
        padRightF   0  '!' ("hello" :: Text) ==#> "hello"
        padRightF   1  '!' ("hello" :: Text) ==#> "hello"
        padRightF   5  '!' ("hello" :: Text) ==#> "hello"
        padRightF   6  '!' ("hello" :: Text) ==#> "hello!"
        padRightF   7  '!' ("hello" :: Text) ==#> "hello!!"
        padRightF   7  '!' (""      :: Text) ==#> "!!!!!!!"
      it "padCenterF" $ do
        padCenterF (-1) '!' ("hello" :: Text) ==#> "hello"
        padCenterF   0  '!' ("hello" :: Text) ==#> "hello"
        padCenterF   1  '!' ("hello" :: Text) ==#> "hello"
        padCenterF   5  '!' ("hello" :: Text) ==#> "hello"
        padCenterF   6  '!' ("hello" :: Text) ==#> "!hello"
        padCenterF   7  '!' ("hello" :: Text) ==#> "!hello!"
        padCenterF   7  '!' ("hell"  :: Text) ==#> "!!hell!"
        padCenterF   7  '!' ("hel"   :: Text) ==#> "!!hel!!"
        padCenterF   8  '!' ("hell"  :: Text) ==#> "!!hell!!"
        padCenterF   8  '!' ("hel"   :: Text) ==#> "!!!hel!!"
        padCenterF   8  '!' (""      :: Text) ==#> "!!!!!!!!"

    describe "integer" $ do
      it "octF" $ do
        octF n ==#> "31"
      it "binF" $ do
        binF n ==#> "11001"
      it "baseF" $ do
        baseF 36 (n^n) ==#> "54kbbzw21jhueg5jb0ggr4p"
      it "-baseF" $ do
        baseF 36 (-(n^n)) ==#> "-54kbbzw21jhueg5jb0ggr4p"

    describe "floating-point" $ do
      let f1_3 = 1.2999999999999998 :: Double
      it "floatF" $ do
        floatF f1_3 ==#> "1.2999999999999998"
      it "exptF" $ do
        exptF 2 f1_3 ==#> "1.30e0"
      it "fixedF" $ do
        fixedF 2 f1_3 ==#> "1.30"
      it "precF" $ do
        precF 2 f1_3 ==#> "1.3"

    describe "conditionals" $ do
      it "whenF" $ do
        whenF True "hi" ==#> "hi"
        whenF False "hi" ==#> ""
      it "unlessF" $ do
        unlessF True "hi" ==#> ""
        unlessF False "hi" ==#> "hi"

    describe "'hexF'" $ do
      it "Int" $ do
        hexF n ==#> "19"
      it "-Int" $ do
        hexF (-n) ==#> "-19"
      it "strict ByteString" $ do
        hexF (BS.pack [15,250]) ==#> "0ffa"
      it "lazy ByteString" $ do
        hexF (BSL.pack [15,250]) ==#> "0ffa"

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

(==%>) :: Text -> Text -> Expectation
(==%>) = shouldBe

(==#>) :: Builder -> Text -> Expectation
(==#>) a b = a `shouldBe` build (T.replace "_" " " b)
