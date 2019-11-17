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

-- Exercise 1, 2, 3

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

  describe "pad" $ do
    it "pad shorter string and capitalize both" $ do
      pad "elephant" "cat" `shouldBe` ("Elephant", "Cat     ")

-- 2.1.
-- - Define 'pad' that pads the shorter of two the strings with trailing spaces
--   and returns both strings capitalized.

--
-- 2.2.
-- - Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
--   The quartiles are elements at the first, second, and third quarter of a list
--   sorted in ascending order. (You can use the built-int 'splitAt' function and
--   the previously defined 'median' function.)
--   quartiles :: [Int] -> (Double,Double,Double)
--   quartiles [3,1,2,4,5,6,8,0,7] => (1.5, 4.0, 6.5)
--
-- Redo Exercise 2 using 'let' instead of 'where'.
--
-- 4.1.
-- - Write a function that takes in a pair (a,b) and a list [c] and returns the
--   following string:
--   "The pair [contains two ones|contains one one|does not contain a single one]
--   and the second element of the list is <x>"
