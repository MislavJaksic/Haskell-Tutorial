module Lecture09Spec
  ( spec
  )
  where

import Test.Hspec

import FER.Course.Lecture09

main :: IO ()
main = hspec spec

simpleDate = D 1 2 3

circleCenter = P 1 2
simpleCircle = Circle circleCenter 2
simpleRectangle = Rectangle (P 5 6) (P 7 8)

transPoint = P 4 4

transCircleCenter = P 7 6
transCircle = Circle transCircleCenter 2
transRectangle = Rectangle (P 3 2) (P 1 0)

spec :: Spec
spec = do

-- Exercise 1

  describe "showDate" $ do
    it "print Date" $ do
      showDate simpleDate `shouldBe` "1.2.3."

  describe "translate" $ do
    it "transplate a circle" $ do
      translate transPoint simpleCircle `shouldBe` transCircle

    it "transplate a rectangle" $ do
      translate transPoint simpleRectangle `shouldBe` transRectangle

  describe "inShape" $ do
    it "point in circle" $ do
      inShape `shouldBe`

    it "point in rectangle" $ do
      inShape `shouldBe` 
