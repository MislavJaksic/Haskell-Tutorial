{-# OPTIONS_GHC -Wall #-}

module FER.Course.Lecture04
  ( headHunter,
    firstColumn,
    shoutOutLoud,
  )
  where

import Data.Char as C

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

pad :: String -> String -> (String, String)
pad x y = if countAndCapitalize x > countAndCapitalize y then else
  where diff as bs = length as - length bs
        countAndCapitalize as = length $ capitalize as
        capitalize (a:as) = C.toUpper a : as

-- Exercise 2

-- Exercise 3
