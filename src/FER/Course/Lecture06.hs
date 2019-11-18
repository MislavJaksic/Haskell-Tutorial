{-# OPTIONS_GHC -Wall #-}

module FER.Course.Lecture06
  ( length',
    maxUnzip,
  )
  where

-- Exercise 1

length' :: [a] -> Int
length' xs = accLength 0 xs
  where accLength n [] = n
        accLength n (_:as) = accLength (n+1) as

maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "Empty list"
maxUnzip ((x, y):xs) = keepMax x y xs
  where keepMax c d [] = (c, d)
        keepMax c d ((a, b):as) = keepMax (max c a) (max d b) as
