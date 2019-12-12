module Lecture08Spec
  ( spec
  )
  where

import Test.Hspec

import FER.Course.Lecture08

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

-- Exercise 1

  describe "sumEven" $ do
    it "sum at even positions" $ do
      sumEven [1..10] `shouldBe` 25

  describe "filterWords" $ do
    it "filter words from string" $ do
      filterWords ["a","b","c","d"] "a b c d e e f g a b c" `shouldBe` "e e f g"

  describe "initials3" $ do
    it "create initials with after filtering" $ do
      initials3 "." (/="that") "a company that makes everything" `shouldBe` "A.C.M.E."

-- Exercise 2

  describe "maxDiff" $ do
    it "max difference between pairs" $ do
      maxDiff [1,2,3,5,1] `shouldBe` 4

  describe "studentsPassed" $ do
    it "return students with score higher then half of max score" $ do
      studentsPassed [("A",0),("B",51),("C",100)] `shouldBe` [("B",51),("C",100)]

-- Exercise 3

  describe "isTitleCased" $ do
    it "all words are capitalized" $ do
      isTitleCased "University Of Zagreb" `shouldBe` True

  describe "sortPairs" $ do
    it "sort by second member of a tuple" $ do
      sortPairs [(2, "world"), (4, "!"), (1, "Hello")] `shouldBe` [(4,"!"),(1,"Hello"),(2,"world")]

  describe "filename" $ do
    it "extract file name from path" $ do
      filename "/etc/init/cron.conf" `shouldBe` "cron.conf"

  describe "maxElemIndices" $ do
    it "find indices of max element" $ do
      maxElemIndices [1,3,4,1,3,4] `shouldBe` [2,5]

-- Exercise 4

  describe "elem'" $ do
    it "is element in list" $ do
      elem' 'a' "abcd" `shouldBe` True

  describe "reverse'" $ do
    it "reverse a list" $ do
      reverse' [1,2,3] `shouldBe` [3,2,1]

  describe "nubRuns" $ do
    it "remove consecutive elements" $ do
      nubRuns "Mississippi" `shouldBe` "Misisipi"

-- Exercise 5

  describe "reverse''" $ do
    it "reverse a list" $ do
      reverse'' [1,2,3] `shouldBe` [3,2,1]

  describe "sumEven'" $ do
    it "sum at even positions" $ do
      sumEven' [1..10] `shouldBe` 25

  describe "maxUnzip" $ do
    it "get max elem in first and second position" $ do
      maxUnzip [(4,1),(2,2),(1,5)] `shouldBe` (4,5)
