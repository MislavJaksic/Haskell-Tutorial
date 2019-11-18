{-# OPTIONS_GHC -Wall #-}

module FER.Course.Lecture07
  ( takeThree,
    dropThree,
    hundredTimes,
    indexFirst,
    indexSecond,
    divider,
    applyOnLast,
    lastTwoPlus100,
    applyManyTimes,
    applyTwice',
    listifylist,
    cutoff,
    sumEvenSquares,
    freq,
    freqFilter,
    withinInterval,
    sndColumn,
    canonicalizePairs,
  )
  where

-- Exercise 1

takeThree :: [a] -> [a]
takeThree = take 3

dropThree :: [a] -> [a]
dropThree = drop 3

hundredTimes :: a -> [a]
hundredTimes = replicate 100

indexFirst :: String -> [(Int, Char)]
indexFirst = zip [0..]

indexSecond :: String -> [(Char, Int)]
indexSecond = flip (zip) [0..]

divider :: Int -> String
divider = flip (replicate) '='

-- Exercise 2

applyOnLast :: (a -> a -> a) -> [a] -> [a] -> a
applyOnLast _ [] _ = error "No head"
applyOnLast _ _ [] = error "No head"
applyOnLast f xs ys = f (last xs) (last ys)

addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 xs ys = addThree 100 (last xs) (last ys)

applyManyTimes :: Int -> (a -> a) -> a -> a
applyManyTimes n f x | n <= 0     = x
                     | otherwise = f (applyManyTimes (n-1) f x)

applyTwice' :: (a -> a) -> a -> a
applyTwice' f = applyManyTimes 2 f

-- Exercise 2

listifylist :: [a] -> [[a]]
listifylist xs = map headList xs
  where headList x = [x]

cutoff :: Int -> [Int] -> [Int]
cutoff n xs = map (min n) xs

-- Exercise 4

sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = sum $ map (\x -> x * x) $ filter (\x -> (x `mod` 2) == 0) xs

freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (\a -> a == x) xs

freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (\x -> (freq x xs) >= n) xs

-- Exercise 5

withinInterval :: Ord a => a -> a -> [a] -> [a]
withinInterval n m xs = filter (\x -> (x >= n) && (x <= m)) xs

sndColumn :: [[a]] -> [a]
sndColumn m = map (\(_:y:_) -> y) m

canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs xs = map (\(a,b) -> if a > b then (b,a) else (a,b)) $ filter (\(a,b) -> a /= b) xs
