{-# OPTIONS_GHC -Wall #-}

module FERExerciseMI
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
    takeFromTo,
    eachThird,
    
    length',
    maxUnzip,
    
    takeThree,
    dropThree,
    hundredTimes,
    index,
    secondIndex,
    applyOnLast,
    lastTwoPlus100,
    applyManyTimes,
    applyTwice',
    listifylist,
    cutoff,
    sumEvenSquares,
    freq,
    withinInterval,
    sndColumn,
    
    sumEven,
    filterWords,
    initials3,
    maxDiff,
    isTitleCased,
    sortPairs
  )
  where



import Data.Char

import FERExerciseMI.Internal



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

-- Exercise 4

report :: (Int, Int) -> String -> String
report (x, y) (_:z:_)
  | (x == 1) && (y == 1) = "The pair contains two ones" ++ sen ++ [z]
  | (x == 1) || (y == 1)  = "The pair contains one" ++ sen ++ [z]
  | otherwise             = "The pair does not contain a single one" ++ sen ++ [z]
    where sen =  " and the second element of the list is "
    
-- Lecture 5

-- Exercise 1

product' :: [Int] -> Int
product' [] = 0
product' (x:[]) = x
product' (x:xs) = x * product' xs

headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf (xs:[]) = head xs : []
headsOf (xs:xss) = head xs : headsOf xss

-- Exercise 2

modMult :: Int -> Int -> [Int] -> [Int]
modMult n m xs = map (mul) xs
  where mul x = x * (n `mod` m)
  
addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = addPrev 0 xs

-- Exercise 3

equalTriplets :: [(Int, Int, Int)] -> [(Int, Int, Int)]
equalTriplets [] = []
equalTriplets (xs:[])
  | isTriple xs = xs : []
  | otherwise   = []
equalTriplets (xs:xss)
  | isTriple xs = xs : equalTriplets xss
  | otherwise   = equalTriplets xss

replicate' :: Int -> a -> [a]
replicate' n x
  | n > 0     = x : replicate' (n-1) x
  | otherwise = []
  
-- Exercise 4

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
  | n > 0     = drop' (n-1) xs
  | otherwise = x:xs

takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo n1 n2 xs
  | n2 > n1   = take (n2-n1+1) (drop n1 xs)
  | otherwise = [] 

  
-- Exercise 5

eachThird :: [a] -> [a]
eachThird [] = []
eachThird xs = everyN 3 xs

-- Lecture 6

-- Exercise 1

length' :: [a] -> Int
length' xs = length'' xs 0
  where length'' [] n = n
        length'' (_:ys) n = length'' ys n+1
        
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "Empty list"
maxUnzip ((c,d):ys) = maxUnzip' ys c d
  where maxUnzip' [] a b = (a, b)
        maxUnzip' ((x,y):xs) a b
          | x > a && y > b = maxUnzip' xs x y
          | x > a          = maxUnzip' xs x b
          | y > b          = maxUnzip' xs a y
          | otherwise      = maxUnzip' xs a b
          
-- Lecture 7

-- Exercise 1

takeThree :: [a] -> [a]
takeThree = take 3

dropThree :: [a] -> [a]
dropThree = drop 3

hundredTimes :: a -> [a]
hundredTimes = replicate 100

index :: [a] -> [(Int, a)]
index = zip [0..]

secondIndex :: [a] -> [(a, Int)]
secondIndex = secondIndexZip

-- Exercise 2

applyOnLast :: (a -> a -> a) -> [a] -> [a] -> a
applyOnLast f xs ys = f (last xs) (last ys)

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 xs ys = applyOnLast (+) xs ys + 100

applyManyTimes :: Int -> (a -> a) -> a -> a
applyManyTimes n f x
  | n > 0     = applyManyTimes (n-1) f (f x)
  | otherwise = x

applyTwice' :: (a -> a) -> a -> a
applyTwice' f x = applyManyTimes 2 f x

-- Exercise 3

listifylist :: [a] -> [[a]]
listifylist = map (\x -> [x])

cutoff :: Int -> [Int] -> [Int]
cutoff n xs = map (min n) xs

-- Exercise 4

sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = sum (map (\x -> x*x) (filter (\x -> (x `mod` 2) == 0) xs))

freq :: Eq a => a -> [a] -> Int
freq x xs = length (filter (x==) xs)

-- Exercise 5

withinInterval :: Int -> Int -> [Int] -> [Int]
withinInterval n m xs = filter (\x -> (m >= x) && (x >= n)) xs

sndColumn :: [[a]] -> [a]
sndColumn = map (\xs -> head (drop 1 xs))

-- Lecture 8

-- Exercise 1

sumEven :: [Integer] -> Integer
sumEven = sum . filter even

filterWords :: [String] -> String -> String
filterWords [] s = s
filterWords _ "" = ""
filterWords (w:ws) s = filterWords ws (filterWord w s)

initials3 :: Char -> (String -> Bool) -> String -> String
initials3 d p s = (delimit d . map (toUpper . head) . filter p) (words s)

-- Exercise 2

maxDiff :: [Int] -> Int
maxDiff [] = 0
maxDiff (_:[]) = 0
maxDiff (x:y:xs) = max (abs (x - y)) (maxDiff (y:xs))

-- Exercise 3

isTitleCased :: String -> Bool
isTitleCased = all isUpper . map head . words

sortPairs :: Ord a => [(a, a)] -> [(a, a)]
sortPairs [] = []
sortPairs (x:[]) = [x]
sortPairs (x:y:xs)
  | (snd x) > (snd y) = y : sortPairs (x:xs)
  | otherwise = x : sortPairs (y:xs)
  
-- filename :: String -> String
-- filename = 
-- filename "/etc/init/cron.conf" => "cron.conf"

-- Exercise 4

-- elem' :: a -> [a] -> Bool
-- elem' x ys = foldr (==) False ys

-- Exercise 5
