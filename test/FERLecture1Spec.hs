module FERLecture1Spec
  ( main
  , spec
  ) where

import Test.Hspec

import FERLecture1
import FERLecture1.Internal



main :: IO ()
main = hspec spec



spec :: Spec
spec = do
  describe "concat3" $ do
    it "concatinate three strings" $ do
      concat3 "ab" "cd" "efg" `shouldBe` "abcdefg"
      
    it "drop the middle string if it is shorter then 2 characters" $ do
      concat3 "1" "2" "3" `shouldBe` "13"
      
  describe "showSalary" $ do
    it "show salary in bonus is not zero" $ do
      showSalary 12345 55 `shouldBe` "Salary is 12345, and a bonus 55"
      
      
      
  describe "dropFirstThree" $ do
    it "drop first three" $ do
      dropFirstThree [1,2,3,4,5] `shouldBe` [4,5]
      
    it "there are too few elements to drop" $ do
      dropFirstThree [1] `shouldBe` []
      
  describe "dropLastThree" $ do
    it "drop last three" $ do
      dropLastThree [1,2,3,4,5] `shouldBe` [1,2]
      
    it "there are too few elements to drop" $ do
      dropLastThree [1] `shouldBe` []
      
      
      
  describe "succN" $ do
    it "apply succ N times to produce d from a" $ do
      succN 3 'a' `shouldBe` 'd'
      
    it "apply succ N times to produce d from z" $ do
      succN 4000 'z' `shouldBe` 'd'
      
      
      
  describe "dropFirstAndLastThree" $ do
    it "drop first and last three" $ do
      dropFirstAndLastThree [1,2,3,4,5,6,7] `shouldBe` [4]
      
    it "there are too few elements to drop" $ do
      dropFirstAndLastThree [1] `shouldBe` []
      
      
      
  describe "initials" $ do
    it "create initials" $ do
      initials "James" "Bond" `shouldBe` "J. B."
      
      
      
  describe "concatLongFirst" $ do
    it "concat long first" $ do
      concatLongFirst "12" "3" `shouldBe` "123"
      
    it "concat long first, but arguments are swapped" $ do
      concatLongFirst "3" "12" `shouldBe` "123"
      
    it "concat first then second if they are the same length" $ do
      concatLongFirst "12" "3" `shouldBe` "123"
      
      
      
  describe "isLeapYear" $ do
    it "1997 is not a leap year" $ do
      isLeapYear 1997 `shouldBe` False
      
    it "1996 is not a leap year" $ do
      isLeapYear 1996 `shouldBe` True
      
    it "1900 is not a leap year" $ do
      isLeapYear 1900 `shouldBe` False
      
    it "2000 is not a leap year" $ do
      isLeapYear 2000 `shouldBe` True
  