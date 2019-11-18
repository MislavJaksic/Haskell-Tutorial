module Lecture05Spec
  ( spec
  )
  where

import Test.Hspec

import FER.Course.Lecture05

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

-- Exercise 1

  describe "recProd" $ do
    it "calc product" $ do
      recProd [2,3,4] `shouldBe` 24

  describe "headsOf" $ do
    it "take heads of lists" $ do
      headsOf [[1,2,3],[4,5],[6]] `shouldBe` [1,4,6]

-- Exercise 2

  describe "modMult" $ do
    it "multiply then module a list" $ do
      modMult 2 3 [1,2,3] `shouldBe` [2,1,0]

  describe "addPredecessor" $ do
    it "add preceding element in the list to the following" $ do
      addPredecessor [3,2,1] `shouldBe` [3,5,3]

-- Exercise 3

  describe "equalTriplets" $ do
    it "return only triples" $ do
      equalTriplets [(1,2,3),(2,2,2),(4,5,6)] `shouldBe` [(2,2,2)]

  describe "replicate'" $ do
    it "replicate element a number of times" $ do
      replicate' 3 'a' `shouldBe` "aaa"

-- Exercise 4

  describe "drop'" $ do
    it "drop elements" $ do
      drop' 2 [1,2,3,4] `shouldBe` [3,4]

    describe "takeFromTo" $ do
      it "splice" $ do
        takeFromTo 1 2 [1,2,3,4] `shouldBe` [2]

-- Exercise 5

  describe "eachThird" $ do
    it "take each third element" $ do
      eachThird "zagreb" `shouldBe` "gb"

  describe "crossZip" $ do
    it "cross zip lists" $ do
      crossZip [1,2,3,4,5] [4,5,6,7,8] `shouldBe` [(1,5),(2,4),(3,7),(4,6)]
