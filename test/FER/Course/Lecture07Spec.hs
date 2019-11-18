module Lecture07Spec
  ( spec
  )
  where

import Test.Hspec

import FER.Course.Lecture07

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

-- Exercise 1

  describe "takeThree" $ do
    it "" $ do
      takeThree [1,2,3,4,5] `shouldBe` [1,2,3]

  describe "dropThree" $ do
    it "" $ do
      dropThree [1,2,3,4,5] `shouldBe` [4,5]

  describe "hundredTimes" $ do
    it "" $ do
      hundredTimes 'a' `shouldBe` "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

  describe "indexFirst" $ do
    it "" $ do
      indexFirst "xyz" `shouldBe` [(0,'x'),(1,'y'),(2,'z')]

  describe "indexSecond" $ do
    it "" $ do
      indexSecond "xyz" `shouldBe` [('x',0),('y',1),('z',2)]

  describe "divider" $ do
    it "" $ do
      divider 4 `shouldBe` "===="

-- Exercise 2

  describe "applyOnLast" $ do
    it "add last two elem of lists" $ do
      applyOnLast (+) [1,2,3] [5,6] `shouldBe` 9

    it "max last two elem of lists" $ do
      applyOnLast max [1,2] [3,4] `shouldBe` 4

  describe "lastTwoPlus100" $ do
    it "add last two and add 100" $ do
      lastTwoPlus100 [1,2,3] [6,5] `shouldBe` 108

  describe "applyManyTimes" $ do
    it "apply a function a number of times to an init value" $ do
      applyManyTimes 5 (+2) 0 `shouldBe` 10

  describe "applyTwice'" $ do
    it "add two twice" $ do
      applyTwice' (+2) 0 `shouldBe` 4

-- Exercise 3

  describe "listifylist" $ do
    it "listify all elements" $ do
      listifylist [1,2,3] `shouldBe` [[1],[2],[3]]

  describe "cutoff" $ do
    it "cutoff elements at 100" $ do
      cutoff 100 [20,202,34,117] `shouldBe` [20,100,34,100]

-- Exercise 4

  describe "sumEvenSquares" $ do
    it "sum squares of even numbers" $ do
      sumEvenSquares [1,2,3,4] `shouldBe` 20

  describe "freq" $ do
    it "how many times it appers in a list" $ do
      freq 'k' "kikiriki" `shouldBe` 3

  describe "freqFilter" $ do
    it "filter elements that occure less then 4 times" $ do
      freqFilter 4 "kikiriki" `shouldBe` "iiii"

-- Exercise 5

  describe "withinInterval" $ do
    it "discard outside the interval" $ do
      withinInterval 2 4 [1,2,3,4,5] `shouldBe` [2,3,4]

  describe "sndColumn" $ do
    it "get second matrix column" $ do
      sndColumn [[1,2,3],[4,5,6]] `shouldBe` [2,5]

  describe "canonicalizePairs" $ do
    it "swap incorrectly placed elements, discard pairs with same elem" $ do
      canonicalizePairs [(4,1),(2,2),(1,5)] `shouldBe` [(1,4),(1,5)]
