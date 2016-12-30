{-# LANGUAGE
OverloadedStrings
  #-}


module Main where


-- Monoid
import Data.Monoid ((<>))
-- Text
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
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
    describe "integer" $ do
      it "hexF" $ do
        (""%<hexF n>%"") ==%> "19"
      it "-hexF" $ do
        (""%<hexF (-n)>%"") ==%> "-19"
      it "octF" $ do
        (""%<octF n>%"") ==%> "31"
      it "binF" $ do
        (""%<binF n>%"") ==%> "11001"
      it "baseF" $ do
        (""%<baseF 36 (n^n)>%"") ==%> "54kbbzw21jhueg5jb0ggr4p"
      it "-baseF" $ do
        (""%<baseF 36 (-(n^n))>%"") ==%> "-54kbbzw21jhueg5jb0ggr4p"
    describe "floating-point" $ do
      let f1_3 = 1.2999999999999998 :: Double
      it "floatF" $ do
        (""%<floatF f1_3>%"") ==%> "1.2999999999999998"
      it "exptF" $ do
        (""%<exptF 2 f1_3>%"") ==%> "1.30e0"
      it "fixedF" $ do
        (""%<fixedF 2 f1_3>%"") ==%> "1.30"
      it "precF" $ do
        (""%<precF 2 f1_3>%"") ==%> "1.3"

  describe "output as" $ do
    it "String" $
      ("a"%<n>%"b" :: String) `shouldBe` "a25b"
    it "Text" $
      ("a"%<n>%"b" :: Text) `shouldBe` "a25b"
    it "Lazy Text" $
      ("a"%<n>%"b" :: TL.Text) `shouldBe` "a25b"
    it "Builder" $
      ("a"%<n>%"b" :: Builder) `shouldBe` "a25b"

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

(==%>) :: Text -> Text -> Expectation
(==%>) = shouldBe
