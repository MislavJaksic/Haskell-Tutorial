module Lecture02Spec
  ( spec
  )
  where

import Test.Hspec

import FER.Course.Lecture02



main :: IO ()
main = hspec spec


sevenIntList :: [Int]
sevenIntList = [0..6]

spec :: Spec
spec = do

-- Exercise 1

  describe "dropFirstThree" $ do
    it "drop first three elements" $ do
      dropFirstThree sevenIntList `shouldBe` [3..6]

    it "drop as much as you can" $ do
      dropFirstThree [0] `shouldBe` []

  describe "dropLastThree" $ do
    it "drop last three elements" $ do
      dropLastThree sevenIntList `shouldBe` [0..3]

    it "drop as much as you can" $ do
      dropLastThree [0] `shouldBe` []

  describe "dropFirstLastThree" $ do
    it "drop first and last three elements" $ do
      dropFirstLastThree sevenIntList `shouldBe` [3]

    it "drop as much as you can" $ do
      dropFirstLastThree [0] `shouldBe` []

  describe "initials" $ do
    it "create initials" $ do
      initials "James" "Bond" `shouldBe` "J. B."

  describe "safeHead" $ do
    it "return empty list if there is no head" $ do
      safeHead [] `shouldBe` ([] :: [Int])

    it "if there is a head, return it" $ do
      safeHead sevenIntList `shouldBe` [0]

  describe "hasDuplicates" $ do
    it "no duplicates" $ do
      hasDuplicates [0,1,2] `shouldBe` False

    it "duplicates" $ do
      hasDuplicates [0,0] `shouldBe` True

-- Exercise 2

  describe "doublesFromTo" $ do
    it "return squares of a sequence" $ do
      doublesFromTo 5 7 `shouldBe` [10,12,14]

    it "negative interval returns an empty list" $ do
      doublesFromTo 7 5 `shouldBe` []

  describe "ceasarCode" $ do
    it "shift letters by n" $ do
      ceasarCode 3 "abcd" `shouldBe` "defg"

    it "make the output lowercase" $ do
      ceasarCode 3 "ABCD" `shouldBe` "defg"

    it "output only characters from 'a' to 'z'" $ do
      ceasarCode 3 "1234" `shouldBe` ""

-- Exercise 3

  describe "letterCount" $ do
    it "count letters in a string" $ do
      letterCount "Helloworld" `shouldBe` 10

    it "don't count whitespaces" $ do
      letterCount "Hel wor" `shouldBe` 6

    it "don't count words shorter then 3 chars" $ do
      letterCount "Hello AB world" `shouldBe` 10

  describe "isPalindrome" $ do
    it "this is a palindrome" $ do
      isPalindrome "12321" `shouldBe` True

    it "this is not a palindrome" $ do
      isPalindrome "12345" `shouldBe` False

    it "ignore case" $ do
      isPalindrome "abcCBA" `shouldBe` True

    it "ignore whitespaces" $ do
      isPalindrome "12 21" `shouldBe` True

  describe "flipp" $ do
    it "revert lists and concatinate them in reverse order" $ do
      flipp ["water","is","warm"] `shouldBe` "mrawsiretaw"

-- Exercise 4

  describe "inCircle" $ do
    it "only the centar is within the circle" $ do
      inCircle 0.1 0 0 `shouldBe` [(0.0, 0.0)]

  describe "steps" $ do
    it "pair even with odd numbered elements but ignore the last unpaied element" $ do
      steps [1,2,3,4,5] `shouldBe` [(1,2), (3,4)]

    it "pair even with odd numbered elements" $ do
      steps [1,2,3,4] `shouldBe` [(1,2), (3,4)]

    it "pair even with odd numbered elements" $ do
      steps [] `shouldBe` ([] :: [(Int, Int)])

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
