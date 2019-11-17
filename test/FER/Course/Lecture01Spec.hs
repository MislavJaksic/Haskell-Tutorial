module Lecture01Spec
  ( spec
  )
  where

import Test.Hspec

import FER.Course.Lecture01

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

-- Exercise 1

  describe "concat3" $ do
    it "concat all three strings" $ do
      concat3 "hello " "world " "hello" `shouldBe` "hello world hello"

    it "drop the middle string because it's shorter then 2 chars" $ do
      concat3 "A" "B" "C" `shouldBe` "AC"

  describe "showSalary" $ do
    it "print amount and bonus" $ do
      showSalary 1 2 `shouldBe` "Salary is 1, and a bonus 2"

    it "print error message because the amount is negative" $ do
      showSalary (-1) 2 `shouldBe` showSalaryNegativeError
