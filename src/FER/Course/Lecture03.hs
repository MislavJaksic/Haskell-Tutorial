{-# OPTIONS_GHC -Wall #-}

module FER.Course.Lecture03
  (
  )
  where

-- Exercise 1

-- Check with "stack ghci", then ":l puh-2017-lecture-03.lhs" and ":t foo10".

-- String -> [String] -- CORRECT
-- foo10 w = [x ++ y | x <- lines w, y <- lines w]

-- String -> [(String, String)]
-- foo11 w = [(x,y) | x <- lines w, y <- lines w]

-- String -> [String]
-- foo12 w = [y : x | x <- lines w, y <- w]

-- String -> [(String, String)]
-- foo13 w = [(y:x, w) | x <- lines w, y <- w]

-- String -> [(Char, Bool)]
-- foo14 w = [(x, x=='a') | x <- w ]

-- String -> String
-- foo15 s = tail [ c | c <- s, isLower c ]

-- String -> [(Char, Char)]
-- foo16 s = zip [ c | c <- s, isLower c ] "Haskell"

-- Integer -> Char -> String
-- foo17 n c = reverse $ drop n $ c : "Haskell"

-- String -> String
-- foo18 xs = last $ words xs

-- Char -> String -> String
-- foo19 x z = x : 'y' : z

-- Exercise 2

-- [a] -> [a]
-- foo20 xs = tail xs ++ [head xs]

-- [a] -> (a, [a])
-- foo21 xs = (head xs, tail xs)

-- a -> [a] -> [a]
-- foo22 x xs = x:xs

-- [a] -> [a]
-- foo23 l = init $ tail l

-- [[a]] -> [a] -> [a]
-- foo24 xss ys = concat xss ++ ys

-- [[a]] -> [b] -> (a, b)
-- foo25 xss ys = (head $ concat xss, head ys)

-- [[[a]]] -> a
-- foo26 xs = head $ concat $ concat xs

-- [a] -> [[a]]
-- foo27 cs = [[c1,c2] | c1 <- cs, c2 <- cs]

-- [[a]] -> [[a]]
-- foo28 cs = [concat [c1,c2] | c1 <- cs, c2 <- cs]

-- [a] -> [a]
-- foo29 cs = concat [[c1,c2] | c1 <- cs, c2 <- cs]

-- Exercise 3

-- Eq a => a -> [a] -> a
-- foo30 x ys = if x == head ys then x else last ys

-- Ord a => a -> [a] -> a
-- foo31 x ys = if x < head ys then x else last ys

-- Eq a => [a] -> [[a]] -> a
-- foo32 xs yss = if xs == head yss then head xs else last xs

-- (Num a, Enum a) => Bool -> [b] -> [(a, b)]
-- foo33 x ys = if x then zip [1..9] ys else []

-- (Num a, Enum a) => String -> [(a, String)]
-- foo34 w = zip [0..] (lines w)

-- (Integral a, Fractional a) => a -> a -> a
-- foo35 x y = if odd x then y else x / 10

-- Ord a => [a] -> [a]
-- foo36 xs = sort xs == xs

-- (Show a, Show b, Foldable t) => a -> t [b] -> [Char]
-- foo37 x xs = show x ++ (show $ concat xs)

-- (Num a, Foldable t) => t [a] -> a
-- foo38 xs = sum $ concat xs

-- (Num a, Ord a) => [a] -> [[a]] -> a
-- foo39 xs yss = sum $ [min x y | x <- xs, ys <- yss, y <- ys]
