module LuhnSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Luhn
import Luhn.Internal



main :: IO ()
main = hspec spec



spec :: Spec
spec = do
  describe "toDigits" $ do
    it "converts a number to a list of digits" $ do
      toDigits 4012888888881882 `shouldBe` [4,0,1,2,8,8,8,8,8,8,8,8,1,8,8,2]
      
    it "converts a negative number to an empty list" $ do
      toDigits (-5) `shouldBe` []
      
    it "converts zero to an empty list" $ do
      toDigits 0 `shouldBe` []
      
  describe "doubleEveryOther" $ do
    it "double every second digit starting from last" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
      
    it "double every second digit starting from last" $ do
      doubleEveryOther [9,1,4] `shouldBe` [9,2,4]
      
  describe "sumDigits" $ do
    it "sum digits of two and one digit number" $ do
      sumDigits [18,7,6,15] `shouldBe` 28
      
    it "sum digits of two and one digit number" $ do
      sumDigits [1,11,111] `shouldBe` 6
      
  describe "lastDigitZero" $ do
    it "last digit isn't zero" $ do
      lastDigitZero 4598 `shouldBe` False
      
    it "last digit is zero" $ do
      lastDigitZero 246970 `shouldBe` True
      
  describe "validate" $ do
    it "valid card number" $ do
      validate 4012888888881881 `shouldBe` True
      
    it "invalid card number" $ do
      validate 4012888888881882 `shouldBe` False
      