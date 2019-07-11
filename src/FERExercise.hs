{-# OPTIONS_GHC -Wall #-}

module FERExercise
  ( letterCount,
    isPalindrome,
    flipp,
    inCircle,
    steps,
    indices,
    showLineNumbers,
    haveAlignment,
    
    headHunter,
    firstColumn,
    shoutOutLoud,
    pad,
    report,
    
    product',
    headsOf,
    modMult,
    addPredecessor,
    equalTriplets,
    replicate',
    drop',
    takeFromTo
  )
  where

import Data.Char

import FERExercise.Internal



-- Lecture 2

-- Exercise 3

letterCount :: String -> Int
letterCount x = sum (map length (filter isOverTwo (words x)))

isPalindrome :: String -> Bool
isPalindrome _ = False

flipp :: [String] -> String
flipp xss = foldl (++) "" (reverse (map reverse xss))

-- Exercise 4

inCircle :: Double -> Double -> Double -> [(Double, Double)]
inCircle r x y = [(a, b) | a <- [-10..10], b <- [-10..10], r >= sqrt ((a - x) ** 2 + (b - y) ** 2)]

steps :: [Int] -> [(Int, Int)]
steps [] = []
steps (_:[]) = []
steps (a:b:[]) = [(a,b)]
steps (a:b:xs) = (a,b) : steps xs

-- Exercise 5

indices :: Char -> String -> [Int]
indices x xs = [i | (i, a) <- (zip [0..] xs), a == x]

showLineNumbers :: String -> String
showLineNumbers xs = unlines [i:' ':a | (i,a) <- zip ['1'..] (lines xs)]

haveAlignment :: String -> String -> Bool
haveAlignment [] [] = False
haveAlignment [] _ = False
haveAlignment _ [] = False
haveAlignment (x:xs) (y:ys) = if x == y then True else haveAlignment xs ys

-- Lecture 4

-- Exercise 1

headHunter :: [[a]] -> a
headHunter [] = error "No heads"
headHunter (xs:xss)
  | not (null xs) = head xs
  | otherwise = headHunter xss

firstColumn :: [[a]] -> [a]
firstColumn [] = []
firstColumn (rs:rss)
  | not (null rs) = (head rs) : firstColumn rss
  | otherwise     = firstColumn rss
  
shoutOutLoud :: String -> String
shoutOutLoud [] = []
shoutOutLoud xs = unwords (map (repeatFirstThreeTimes) (words xs))

-- Exercise 2

pad :: String -> String -> (String, String)
pad xs ys = (capitalize xs, capitalize ys ++ "    ")
  where capitalize [] = []
        capitalize (z:zs) = (toUpper z) : zs

-- quartiles :: [Int] -> (Double,Double,Double)
-- quartiles x = 

-- Exercise 4

report :: (Int, Int) -> String -> String
report (x, y) (_:z:_)
  | (x == 1) && (y == 1) = "The pair contains two ones" ++ sen ++ [z]
  | (x == 1) || (y == 1)  = "The pair contains one" ++ sen ++ [z]
  | otherwise             = "The pair does not contain a single one" ++ sen ++ [z]
    where sen =  " and the second element of the list is "
    
