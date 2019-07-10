module FERExerciseSpec
  ( main
  , spec
  ) where

import Test.Hspec

import FERExercise
import FERExercise.Internal



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

-- Lecture 2

-- Exercise 3

  describe "letterCount" $ do
    it "count characters while ignoring whitespaces" $ do
      letterCount " abc defg " `shouldBe` 7
    
    it "count characters while ignoring words with less then three characters" $ do
      letterCount "ab cdefg" `shouldBe` 5
      
    it "count characters while ignoring both short words and whitespaces" $ do
      letterCount "A Red Fox" `shouldBe` 6
      
  describe "flipp" $ do
    it "reverse each string, reverse the order of string then concatinate them" $ do
      flipp ["water","is","warm"] `shouldBe` "mrawsiretaw"
      
-- Exercise 4

  -- describe "inCircle" $ do
    -- it "only the center is within the circle" $ do
      -- inCircle 0.1 0 0 `shouldBe` [0.0, 0.0]
      
  describe "steps" $ do
    it "pair even with odd numbered elements but ignore the last unpaied element" $ do
      steps [1,2,3,4,5] `shouldBe` [(1,2), (3,4)]
      
    it "pair even with odd numbered elements" $ do
      steps [1,2,3,4] `shouldBe` [(1,2), (3,4)]
      
-- Exercise 5

  describe "indices" $ do
    it "get element indices" $ do
      indices 'a' "alphabet" `shouldBe` [0, 4]
      
  describe "showLineNumbers" $ do
    it "prefix each line with a number" $ do
      showLineNumbers "first line\nsecond line" `shouldBe` "1 first line\n2 second line\n"
      
  describe "haveAlignment" $ do
    it "there is a character in both strings that aligns" $ do
      haveAlignment "water" "fire" `shouldBe` True
      
      
      
  describe "isOverTwo" $ do
    it "is longer then two characters" $ do
      isOverTwo "123" `shouldBe` True
      
    it "isn't longer then two characters" $ do
      isOverTwo "12" `shouldBe` False
      

