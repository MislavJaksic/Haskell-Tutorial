{-# OPTIONS_GHC -Wall #-}

module FER.Course.Lecture09
  ( Date(D),
    showDate,
    Point(P),
    Shape(Circle, Rectangle),
    translate,
  )
  where

-- Exercise 1

data Date = D Int Int Int deriving (Show, Eq)

showDate :: Date -> String
showDate (D d m y) = numDot d ++ numDot m ++ numDot y
  where numDot x = show x ++ "."

data Point = P Double Double
  deriving (Show, Eq)
data Shape = Circle Point Double |
             Rectangle Point Point
  deriving (Show, Eq)

translate :: Point -> Shape -> Shape
translate pt (Rectangle pr1 pr2) = Rectangle (translatePoint pt pr1) (translatePoint pt pr2)
translate pt (Circle pc c) = Circle (translatePoint pt pc)  c

translatePoint :: Point -> Point -> Point
translatePoint (P tx ty) (P a b) = P (2 * tx - a) (2 * ty - b)

-- inShape :: Shape -> Point -> Bool
-- inShape

-- inShapes :: [Shape] -> Point -> Bool
-- inShapes
--
-- data Vehicle = Car String Double | Truck String Double | Motorcycle String Double | Bicycle
--
-- totalHorsepower ::
-- totalHorsepower xs =
