{-# OPTIONS_GHC -Wall #-}

module FER.Course.Lecture05
  ( recProd,
    headsOf,
    modMult,
    addPredecessor,
    equalTriplets,
    replicate',
    drop',
    takeFromTo,
    eachThird,
    crossZip,
  )
  where

-- Exercise 1

recProd :: Num a => [a] -> a
recProd [] = 0
recProd (x:[]) = x
recProd (x:xs) = x * recProd xs

headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf (xs:xss) = head xs : headsOf xss

-- Exercise 2

modMult :: (Integral a, Num a) => a -> a -> [a] -> [a]
modMult _ _ [] = []
modMult n m (x:xs) = (x * n `mod` m) : modMult n m xs

addPredecessor :: Num a => [a] -> [a]
addPredecessor [] = []
addPredecessor (x:xs) = x : addPrevious x xs
  where addPrevious _ [] = []
        addPrevious b (a:as) = a + b : addPrevious a as

-- Exercise 3

equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets [] = []
equalTriplets (xs:xss) | isTriple xs = xs : equalTriplets xss
                       | otherwise = equalTriplets xss
  where isTriple (a,b,c) = (a == b) && (b == c)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- Exercise 4

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo n1 n2 xs = take (n2-n1) (drop' n1 xs)

-- Exercise 5

eachThird :: [a] -> [a]
eachThird [] = []
eachThird (_:[]) = []
eachThird (_:_:[]) = []
eachThird (_:_:z:xs) = z : eachThird xs

crossZip :: [a] -> [a] -> [(a, a)]
crossZip [] _ = []
crossZip _ [] = []
crossZip (_:[]) (_:[]) = []
crossZip [_] (_:_:_) = []
crossZip (_:_:_) [_] = []
crossZip (a1:a2:as) (b1:b2:bs) = (a1, b2) : (a2, b1) : crossZip as bs
