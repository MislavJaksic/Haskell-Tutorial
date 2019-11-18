module Lecture06Spec
  ( spec
  )
  where

import Test.Hspec

import FER.Course.Lecture06

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

-- Exercise 1

  describe "length'" $ do
    it "accumulator length" $ do
      length' [1,2,3] `shouldBe` 3

  describe "maxUnzip" $ do
    it "accumulator unzip" $ do
      maxUnzip [(1,5), (4,0), (2,3)] `shouldBe` (4,5)
