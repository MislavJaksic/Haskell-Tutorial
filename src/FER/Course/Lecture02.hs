{-# OPTIONS_GHC -Wall #-}

module FER.Course.Lecture02
  ( dropFirstThree,
    dropLastThree,
    dropFirstLastThree,
    initials,
    safeHead,
    hasDuplicates,

    doublesFromTo,
    ceasarCode,

    letterCount,
    wordCount,
    isPalindrome,
    flipp,

    inCircle,
    steps,

    indices,
    showLineNumbers,
    haveAlignment
  )
  where

import qualified Data.List as L
import qualified Data.Char as C

-- Exercise 1

dropFirstThree :: [a] -> [a]
dropFirstThree xs = drop 3 xs

dropLastThree :: [a] -> [a]
dropLastThree xs = take (length xs - 3) xs

dropFirstLastThree :: [a] -> [a]
dropFirstLastThree xs = dropFirstThree $ dropLastThree xs

initials :: String -> String -> String
initials xs ys = head xs : '.' : ' ' : head ys : '.' : []

safeHead :: [a] -> [a]
safeHead [] = []
safeHead xs = [head xs]

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates xs = (L.nub xs) /= xs

-- Exercise 2

doublesFromTo :: (Num a, Enum a) => a -> a -> [a]
doublesFromTo x y = [a + a | a <- [x..y]]

ceasarCode :: Int -> String -> String
ceasarCode n xs = [C.toLower ceasarChar | x <- xs, let ceasarChar = iterate succ x !! n, (C.isLetter $ ceasarChar) == True]

-- Exercise 3

letterCount :: String -> Int
letterCount xs = wordCount $ words xs

wordCount :: [String] -> Int
wordCount = sum . filter (>2) . map length

isPalindrome :: String -> Bool
isPalindrome x = filtered == reverse(filtered)
  where filtered = map C.toLower $ filter (/=' ') x

flipp :: [String] -> String
flipp = foldl (++) [] . reverse . map reverse

-- Exercise 4

inCircle :: Double -> Double -> Double -> [(Double, Double)]
inCircle r x y = [(a, b) | a <- [-10..10], b <- [-10..10], r >= sqrt ((a - x) ** 2 + (b - y) ** 2)]

steps :: [a] -> [(a, a)]
steps [] = []
steps (_:[]) = []
steps (a:b:[]) = [(a,b)]
steps (a:b:xs) = (a,b) : steps xs

-- Exercise 5

indices :: Char -> String -> [Int]
indices x xs = [i | (i, a) <- (zip [0..] xs), a == x]

showLineNumbers :: String -> String
showLineNumbers xs = unlines [i:' ':a | (i, a) <- zip ['1'..] (lines xs)]

haveAlignment :: (Eq a) => [a] -> [a] -> Bool
haveAlignment xs ys = or [x == y | (x, y) <- (zip xs ys)]
