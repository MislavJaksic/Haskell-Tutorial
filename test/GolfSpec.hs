module GolfSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Golf



main :: IO ()
main = hspec spec



spec :: Spec
spec = do
  describe "a" $ do
    it "" $ do
      "" `shouldBe` ""