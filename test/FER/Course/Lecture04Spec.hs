module Lecture04Spec
  ( spec
  )
  where

import Test.Hspec

import FER.Course.Lecture04

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

-- Exercise 1

  describe "headHunter" $ do
    it "head of first list" $ do
      headHunter [[1,2,3]] `shouldBe` 1

    it "head of second list" $ do
      headHunter [[],[1,2,3]] `shouldBe` 1

  describe "firstColumn" $ do
    it "get the head of each int list" $ do
      firstColumn [[1,2],[3,4],[5,6]] `shouldBe` [1,3, 5]

    it "get the head of each char list" $ do
      firstColumn [['a','b'],['c','d']] `shouldBe` ['a','c']

  describe "shoutOutLoud" $ do
    it "triple head of each word" $ do
      shoutOutLoud "Is anybody here?" `shouldBe` "IIIs aaanybody hhhere?"

-- Exercise 2

  describe "pad" $ do
    it "pad shorter string and capitalize both" $ do
      pad "elephant" "cat" `shouldBe` ("Elephant", "Cat     ")

  -- describe "quartiles" $ do
  --   it "computer 1., 2. and 3. quartiles" $ do
  --     [3,1,2,4,5,6,8,0,7] `shouldBe` (1.5, 4.0, 6.5)

  describe "explainTupleList" $ do
    it "explain input" $ do
      explainTupleList (1,2) ['a','b','c'] `shouldBe` "The pair contains one one and the second element of the list is b"
