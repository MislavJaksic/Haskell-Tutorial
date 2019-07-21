{-# OPTIONS_GHC -Wall #-}

module FERLecture1
  ( concat3,
    showSalary,
    
    dropFirstAndLastThree,
    initials,
    concatLongFirst,
    safeHead,
    hasDuplicates,
    doublesFromTo,
    
    
    leapList,
    isLeapYear
  )
  where

import FERLecture1.Internal

import Data.List



-- Exercise, lecture 1

-- 1

concat3 :: String -> String -> String -> String
concat3 x y z
  | (length y) < 2 = x ++ z
  | otherwise = x ++ y ++ z

showSalary :: Int -> Int -> String
showSalary amount bonus = if bonus /= 0
                          then "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus
                          else "Salary is " ++ show amount
                          
-- Exercise, lecture 2

-- 1

dropFirstAndLastThree :: [a] -> [a]
dropFirstAndLastThree xs = dropFirstThree $ dropLastThree xs

initials :: String -> String -> String
initials x y = take 1 x ++ ". " ++ take 1 y ++ "."

concatLongFirst :: String -> String -> String
concatLongFirst x y
  | length x >= length y = x ++ y
  | otherwise           = y ++ x

safeHead :: [a] -> [a]
safeHead [] = []
safeHead xs = [head xs]

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates xs = not ((nub xs) == xs)

-- 2

doublesFromTo :: Double -> Double -> [Double]
doublesFromTo a b
  | a > b     = [x*x | x <- [b,b+1..a]]
  | otherwise = [x*x | x <- [a,a+1..b]]

--ceasarCode :: Int -> String -> String
--ceasarCode n xs = [repeat n (succ y) | y <- xs]

-- Homework

leapList :: [Int]
leapList = [x | x <- [1996,1997..2007], isLeapYear x]

isLeapYear :: Int -> Bool
isLeapYear year
  | (year `mod` 4) /= 0 = False
  | (year `mod` 100) /= 0 = True
  | (year `mod` 400) /= 0 = False
  | otherwise = True

  
  
-- evaluate :: Double -> [Double] -> Double
-- evaluate x aks = foldr (+) [a*x | a <- aks]