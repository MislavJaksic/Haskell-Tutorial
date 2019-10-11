module LYAH.AIntroductionSpec
  ( spec
  )
  where

import Test.Hspec

import LYAH.AIntroduction



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "hello" $ do
    it "print hello" $ do
      hello `shouldBe` "hello"
