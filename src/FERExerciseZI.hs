{-# OPTIONS_GHC -Wall #-}

module FERExerciseZI
  ( showDate,
    totalHorsepower,
    improveStudent
  )
  where



import FERExerciseZI.Internal



-- Lecture 9

-- Exercise 1

showDate :: Date -> String
showDate (Date d m y) = intDot d ++ intDot m ++ intDot y
  where intDot x = show x ++ "."

-- translate :: Point -> Shape2 -> Shape2
-- translate p s = 

-- inShape :: Shape2 -> Point -> Bool
-- inShape s p = 

-- inShapes :: [Shape2] -> Point -> Bool
-- inShapes ss p = 
  
totalHorsepower :: [Vehicle] -> Double
totalHorsepower [] = 0.0
totalHorsepower (x:xs) = getHorsepower x + totalHorsepower xs

-- Exercise 2

improveStudent :: Student -> Student
improveStudent s = s { avgGrade = (min (avgGrade s + 1.0) 5.0) }

-- avgGradePerLevels :: [Student] -> (Double, Double, Double)
-- avgGradePerLevels xs = 

-- rankedStudents :: Level -> [Students] -> [String]
-- rankedStudents lvl xs =

-- Exercise 3

-- Exercise 4

-- Exercise 5
