{-# OPTIONS_GHC -Wall #-}

module FERExercise.Internal
  ( isOverTwo,
    
    repeatFirstThreeTimes,
    
    addPrev,
    isTriple,
    -- everyN
    secondZip
  )
  where



-- Lecture 2

isOverTwo :: String -> Bool
isOverTwo x = (length x) > 2

-- Lecture 4

repeatFirstThreeTimes :: [a] -> [a]
repeatFirstThreeTimes [] = []
repeatFirstThreeTimes (x:[]) = x:x:x:[]
repeatFirstThreeTimes (x:xs) = x:x:x:xs

-- Lecture 5

addPrev :: Num a => a -> [a] -> [a]
addPrev _ [] = []
addPrev p (x:[]) = (x + p) : []
addPrev p (x:xs) = (x + p) : addPrev x xs

isTriple :: (Int, Int, Int) -> Bool
isTriple (x,y,z)
  | (x == y) && (x == z) = True
  | otherwise            = False
  
-- everyN :: Int -> [a] -> [a]
-- everyN _ [] = []
-- everyN n xs
  -- | n < 1            = []
  -- | (length xs) >= n = (head (drop (n-1) xs)) : everyN n (drop (n-1) xs)
  -- | otherwise        = []

