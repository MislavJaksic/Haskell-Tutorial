{-# OPTIONS_GHC -Wall #-}

module FER.Course.Lecture04
  ( headHunter,
    firstColumn,
    shoutOutLoud,
    pad,
    -- quartiles,
    explainTupleList,
  )
  where

import Data.Char as C
import Data.List as L

-- Exercise 1

headHunter :: [[a]] -> a
headHunter [] = error "No head"
headHunter ([]:xss) = headHunter xss
headHunter (xs:_) = head xs

firstColumn :: [[a]] -> [a]
firstColumn [] = error "Not a matrix"
firstColumn xss = [head xs | xs <- xss]

shoutOutLoud :: String -> String
shoutOutLoud s = unwords [head w : head w : w | w <- words s]

-- Exercise 2

pad :: String -> String -> (String, String)
pad x y = if length x > length y then padAndCap x y else padAndCap y x
  where padAndCap a b = (capitalize a, capitalize $ padToLength b (diff a b))
        diff as bs = length as - length bs
        padToLength a n = a ++ replicate n ' '
        capitalize [] = []
        capitalize (a:as) = C.toUpper a : as

median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = L.sort xs

-- quartiles :: [Int] -> (Double,Double,Double)
-- quartiles xs =

-- Exercise 3

-- Skipped: rewrite exercise 2 using 'let'

-- Exercise 4

explainTupleList :: (Integer, Integer) -> [Char] -> String
explainTupleList (a, b) (x:y:cs) = pair ++ tupleOutcome (a, b) ++ secondEle ++ [y]
  where pair = "The pair "
        tupleOutcome (1, 1) = "contains two ones"
        tupleOutcome (_, 1) = "contains one one"
        tupleOutcome (1, _) = "contains one one"
        tupleOutcome (_, _) = "does not contain a single one"
        secondEle = " and the second element of the list is "
