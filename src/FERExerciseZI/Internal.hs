{-# OPTIONS_GHC -Wall #-}

module FERExerciseZI.Internal
  ( Date(Date),
    Vehicle(Car, Truck, Motorcycle, Bicycle),
    getHorsepower,
    Level(Bachelor, Master, PhD),
    Student(Student, firstName, lastName, studentId, level, avgGrade),
    avgStudent,
    avgStudentImp,
    topStudent,
    studentsByLevel,
    avgStudentGrade
  )
  where



-- Lecture 9

data Date = Date Int Int Int deriving Show

data Point = Point Double Double deriving Show
data Shape2 = Circle2 Point Double 
            | Rectangle2 Point Point deriving Show

data Vehicle = 
    Car String Double
  | Truck String Double
  | Motorcycle String Double
  | Bicycle

getHorsepower :: Vehicle -> Double
getHorsepower Bicycle          = 0.2
getHorsepower (Car _ h)        = h
getHorsepower (Truck _ h)      = h
getHorsepower (Motorcycle _ h) = h

data Level = Bachelor 
           | Master 
           | PhD deriving (Show, Eq)

data Student = Student
  { firstName  :: String
  , lastName   :: String
  , studentId  :: String
  , level      :: Level
  , avgGrade   :: Double } deriving (Show, Eq)

avgStudent :: Student
avgStudent = Student { firstName = "A"
                     , lastName = "A"
                     , studentId = "0036"
                     , level = Master
                     , avgGrade = 3.5 }

avgStudentImp :: Student
avgStudentImp = avgStudent { avgGrade = 4.5 }
                        
topStudent :: Student
topStudent = avgStudentImp { avgGrade = 5.0 }

studentsByLevel :: [Student] -> Level -> [Student]
studentsByLevel xs lvl = filter (\x -> (level x) == lvl) xs

avgStudentGrade :: [Student] -> Double
avgStudentGrade [] = 0.0
avgStudentGrade xs = (foldl (+) 0 (map avgGrade xs)) / (fromIntegral (length xs))
