module FERExerciseZISpec
  ( main
  , spec
  ) where

import Test.Hspec

import FERExerciseZI
import FERExerciseZI.Internal



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

-- Lecture 9

-- Exercise 1

  -- describe "" $ do
    -- it "" $ do
      -- f `shouldBe` 
      
  describe "showDate" $ do
    it "show date is format DD.MM.YYYY" $ do
      showDate (Date 7 7 1997) `shouldBe` "7.7.1997."
      
  describe "totalHorsepower" $ do
    it "sum Vehicle horsepower" $ do
      totalHorsepower [Bicycle, Motorcycle "car_man" 0.3, Truck "another_man" 0.5] `shouldBe` 1.0
      
    it "no Vehicles have no horsepower" $ do
      totalHorsepower [] `shouldBe` 0.0

  describe "improveStudent" $ do
    it "improve student score by 1" $ do
      improveStudent avgStudent `shouldBe` avgStudentImp
      
    it "improve student score and hit the ceiling of 5.0" $ do
      improveStudent avgStudentImp `shouldBe` topStudent
      
  describe "improveStudent" $ do
    it "improve student score by 1" $ do
      improveStudent avgStudent `shouldBe` avgStudentImp
      
    it "improve student score and hit the ceiling of 5.0" $ do
      improveStudent avgStudentImp `shouldBe` topStudent
