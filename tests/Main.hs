{-# LANGUAGE
OverloadedStrings
  #-}


module Main where


-- Monoid
import Data.Monoid ((<>))
-- Text
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, fromLazyText)
-- Various data structures
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Map as M
import Data.Map (Map)
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
      let unlinesB = fromLazyText . TL.unlines
      it "empty list" $ do
        blockListF ([] :: [Int]) ==#> "[]\n"
      it "null elements" $ do
        blockListF ([""] :: [Text]) ==#>
          unlinesB ["-"]
        blockListF (["",""] :: [Text]) ==#>
          unlinesB ["-",
                    "-"]
        blockListF (["","a",""] :: [Text]) ==#>
          unlinesB ["-",
                    "- a",
                    "-"]
      it "single-line elements" $ do
        blockListF (["a"] :: [Text]) ==#>
          unlinesB ["- a"]
        blockListF (["a","b"] :: [Text]) ==#>
          unlinesB ["- a",
                    "- b"]
        blockListF (["a","b","ccc"] :: [Text]) ==#>
          unlinesB ["- a",
                    "- b",
                    "- ccc"]
      it "multi-line elements" $ do
        blockListF (["a\nx"] :: [Text]) ==#>
          unlinesB ["- a",
                    "  x"]
        blockListF (["a\n x"] :: [Text]) ==#>
          unlinesB ["- a",
                    "   x"]
        blockListF (["a\n x"," b\nxx\ny y ","c\n\n"] :: [Text]) ==#>
          unlinesB ["- a",
                    "   x",
                    "",
                    "-  b",
                    "  xx",
                    "  y y ",
                    "",
                    "- c",
                    "  "]
      it "mix of single-line and multi-line" $ do
        blockListF (["a\nx","b"] :: [Text]) ==#>
          unlinesB ["- a",
                    "  x",
                    "",
                    "- b"]
        blockListF (["a\nx","b\n"] :: [Text]) ==#>
          unlinesB ["- a",
                    "  x",
                    "",
                    "- b"]
        blockListF (["a"," b\nxx\ny y ","c\n\n"] :: [Text]) ==#>
          unlinesB ["- a",
                    "",
                    "-  b",
                    "  xx",
                    "  y y ",
                    "",
                    "- c",
                    "  "]

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
      let unlinesB = fromLazyText . TL.unlines
      it "empty map" $ do
        blockMapF ([] :: [(Int, Int)]) ==#> "{}\n"
      it "complex example" $ do
        blockMapF ([("hi", ""),
                    ("foo"," a\n  b"),
                    ("bar","a"),
                    ("baz","a\ng")] :: [(Text, Text)]) ==#>
          unlinesB ["hi:",
                    "foo:",
                    "   a",
                    "    b",
                    "bar: a",
                    "baz:",
                    "  a",
                    "  g"]

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
      it "hexF" $ do
        hexF n ==#> "19"
      it "-hexF" $ do
        hexF (-n) ==#> "-19"
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

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

(==%>) :: Text -> Text -> Expectation
(==%>) = shouldBe

(==#>) :: Builder -> Builder -> Expectation
(==#>) = shouldBe
