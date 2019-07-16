module FERExerciseMISpec
  ( main
  , spec
  ) where

import Test.Hspec

import FERExerciseMI
import FERExerciseMI.Internal



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

  describe "inCircle" $ do
    it "only the center is within the circle" $ do
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
      
      
      
  describe "isOverTwo" $ do
    it "is longer then two characters" $ do
      isOverTwo "123" `shouldBe` True
      
    it "isn't longer then two characters" $ do
      isOverTwo "12" `shouldBe` False
      
-- Lecture 4

-- Exercise 1

  describe "headHunter" $ do
    it "get the first head" $ do
      headHunter [[], [], [1,2], [3,4]] `shouldBe` 1
      
  describe "firstColumn" $ do
    it "get the first matrix column" $ do
      firstColumn [[1,2],[3,4]] `shouldBe` [1,3]

  describe "shoutOutLoud" $ do
    it "repeat the first three letters of each word" $ do
      shoutOutLoud "Is anybody here?" `shouldBe` "IIIs aaanybody hhhere?"   
      
-- Exercise 2

  describe "pad" $ do
    it "capitalize both and add trailing spaces to the second word" $ do
      pad "elephant" "cat" `shouldBe` ("Elephant", "Cat    ")
      
-- Lecture 5

-- Exercise 1

  describe "product'" $ do
    it "product of the function" $ do
      product' [1,2,3,4] `shouldBe` 24
      
  describe "headsOf" $ do
    it "get the heads of lists" $ do
      headsOf [[1,2,3],[4,5],[6]] `shouldBe` [1,4,6]
      
-- Exercise 2

  describe "addPredecessor" $ do
    it "add the previous element to the current element" $ do
      addPredecessor [3,2,1] `shouldBe` [3,5,3]
      
-- Exercise 3

  describe "equalTriplets" $ do
    it "filter triplets for which x=y=z in (x,y,z)" $ do
      equalTriplets [(1,2,3),(2,2,2),(4,5,6)] `shouldBe` [(2,2,2)]
      
  describe "replicate'" $ do
    it "non negative replication" $ do
      replicate' 3 'a' `shouldBe` ['a','a','a']
      
-- Exercise 4

  describe "drop'" $ do
    it "drop a number of list elements" $ do
      drop' 2 [1,2,3,4,5] `shouldBe` [3,4,5]
      
  describe "takeFromTo" $ do
    it "get elements in range" $ do
      takeFromTo 1 3 [0,1,2,3,4] `shouldBe` [1,2,3]
      
-- Exercise 5

  describe "eachThird" $ do
    it "take every third element" $ do
      eachThird "zagreb" `shouldBe` "gb"
      
      
      
  describe "everyN" $ do
    it "take every element" $ do
      everyN 1 "zagreb" `shouldBe` "zagreb"
      
    it "take every second element" $ do
      everyN 2 "zagreb" `shouldBe` "arb"
      
    it "take every third element" $ do
      everyN 3 "zagreb" `shouldBe` "gb"
      
    it "there is nothing to take" $ do
      everyN 3 "" `shouldBe` ([] :: String)
      
    it "takes nothing" $ do
      everyN (-3) "zagreb" `shouldBe` ([] :: String)
      
-- Lecture 6

-- Exercise 1

  describe "length'" $ do
    it "count elements" $ do
      length' "zagreb" `shouldBe` 6
      
    it "count nothing" $ do
      length' [] `shouldBe` 0
      
  describe "maxUnzip" $ do
    it "get both max elements" $ do
      maxUnzip [(1,4),(2,3),(3,2)] `shouldBe` (3,4)
      
    it "there is only one element" $ do
      maxUnzip [(1,4)] `shouldBe` (1,4)

-- Lecture 7

-- Exercise 1

  describe "index" $ do
    it "first place indexed elements" $ do
      index "xyz" `shouldBe` [(0,'x'),(1,'y'),(2,'z')]
      
  describe "secondIndex" $ do
    it "second place indexed elements" $ do
      secondIndex "xyz" `shouldBe` [('x',0),('y',1),('z',2)]

-- Lecture 8

-- Exercise 1

  describe "sumEven" $ do
    it "sum even elements" $ do
      sumEven [1,2,3,4,5,6] `shouldBe` 12
      
  describe "filterWords" $ do
    it "" $ do
      filterWords ["red", "brown", "a"] "a red fox jumps over a brown fence" `shouldBe` "fox jumps over fence"
      
  describe "initials3" $ do
    it "capitalize letters, delimit and filter words by a predicate" $ do
      initials3 '.' (/="that") "a company that makes everything" `shouldBe` "A.C.M.E."

-- Exercise 2

  describe "maxDiff" $ do
    it "sum even elements" $ do
      maxDiff [1,2,3,5,1] `shouldBe` 4
      
-- Exercise 3

  describe "isTitleCased" $ do
    it "the first letter of every word is capital" $ do
      isTitleCased "University Of Zagreb" `shouldBe` True
      
  describe "sortPairs" $ do
    it "sort the list of pairs in ascending order with respect to the second element" $ do
      sortPairs [(0,3), (5,1), (10,2)] `shouldBe` [(5,1), (10,2), (0,3)]
      
      
      
      
      
  describe "filterWord" $ do
    it "filter word from string" $ do
      filterWord "hello" "hello world" `shouldBe` "world"
      
  describe "delimit" $ do
    it "delimit characters with a delimiter" $ do
      delimit '.' "ACME" `shouldBe` "A.C.M.E."
      