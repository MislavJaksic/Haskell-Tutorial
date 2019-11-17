module Lecture03Spec
  ( spec
  )
  where

import Test.Hspec

import FER.Course.Lecture03

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

-- Exercise 1, 2, 3

  describe "no tests to run" $ do
    it "always True" $ do
      True `shouldBe` True

-- Types cannot be unit tested.
