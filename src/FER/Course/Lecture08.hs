{-# OPTIONS_GHC -Wall #-}

module FER.Course.Lecture08
  ( sumEven,
    filterWords,
    initials3,
    maxDiff,
    studentsPassed,
    isTitleCased,
    sortPairs,
    filename,
    maxElemIndices,
    elem',
    reverse',
    nubRuns,
    reverse'',
    sumEven',
    maxUnzip,
  )
  where

import Data.Char as C
import Data.List as L
import Data.List.Split as S

-- Exercise 1

sumEven :: [Integer] -> Integer
sumEven = sum . filterEvenPos
  where filterEvenPos [] = []
        filterEvenPos (_:[]) = []
        filterEvenPos (x:_:xs) = x : filterEvenPos xs

filterWords :: [String] -> String -> String
filterWords [] s = s
filterWords (w:ws) s = filterWords ws (unwords (filter (/=w) (words s)))

initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concat . map (\x -> x : d) . map (\x -> C.toUpper $ head x) . filter p . words

-- Exercise 2

maxDiff :: [Int] -> Int
maxDiff [] = 0
maxDiff (_:[]) = 0
maxDiff (x:y:xs) = max (abs x - y) (maxDiff (y:xs))

studentsPassed :: [(String, Float)] -> [(String, Float)]
studentsPassed [] = []
studentsPassed xs = filter (\(_,b) -> b > ((findMax xs) / 2)) xs
  where findMax [] = error "Empty list"
        findMax ((_,b):[]) = b
        findMax ((_,b):as) = max b (findMax as)

-- Exercise 3

isTitleCased :: String -> Bool
isTitleCased = all (\x -> C.isUpper $ head x) . words

sortPairs :: Ord b => [(a, b)] -> [(a, b)]
sortPairs = L.sortBy (\(_,a) (_,b) -> compare a b)

filename :: String -> String
filename = last . S.splitOn "/"

maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices [] = error "Empty list"
maxElemIndices xs = findIndices (\x -> x == (findMax xs)) xs
  where findMax [] = error "Empty list"
        findMax (a:[]) = a
        findMax (a:as) = max a (findMax as)

-- Exercise 4

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (||) False . map (==x)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = foldr (\a acc -> acc ++ [a]) [] xs

nubRuns :: Eq a => [a] -> [a]
nubRuns [] = []
nubRuns xs = foldr (\a (b:acc) -> if a == b then b : acc else a : b : acc) [last xs] (init xs)

-- Exercise 5

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

sumEven' :: Num a => [a] -> a
sumEven' [] = 0
sumEven' xs = foldl (+) 0 (filterEvenPos xs)
  where filterEvenPos [] = []
        filterEvenPos (_:[]) = []
        filterEvenPos (a:_:as) = a : filterEvenPos as

maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip [] = error "Empty list"
maxUnzip (x:xs) = foldl maxTuple x xs
  where maxTuple (a,b) (c,d) | a > c && b > d = (a,b)
                             | a > c && b < d = (a,d)
                             | a < c && b > d = (c,b)
                             | otherwise      = (c,d)
