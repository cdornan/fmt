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
  let n = 25 :: Int
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

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

(==%>) :: Text -> Text -> Expectation
(==%>) = shouldBe
