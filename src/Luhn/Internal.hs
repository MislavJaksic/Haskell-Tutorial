module Luhn.Internal
  ( toDigits
  , doubleEveryOther
  , sumDigits
  , lastDigitZero
  )
  where


  
lastDigitZero :: Int -> Bool
lastDigitZero x = lastDigit x == 0



sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs) = foldr (+) 0 (toDigits x) + sumDigits xs



doubleEveryOther :: [Int] -> [Int]
doubleEveryOther xs = reverse $ doubleEveryOtherInternal 1 (reverse xs)

doubleEveryOtherInternal :: Int -> [Int] -> [Int]
doubleEveryOtherInternal _ [] = []
doubleEveryOtherInternal n (x:xs) = (if n `mod` 2 == 0 then double x else x) : (doubleEveryOtherInternal (n + 1) xs)

double :: Int -> Int
double x = x * 2



toDigits :: Int -> [Int]
toDigits x = reverse $ toDigitsRev x

toDigitsRev :: Int -> [Int]
toDigitsRev x
  | x < 1 = []
  | otherwise = lastDigit x : (toDigitsRev $ tens x)

lastDigit :: Int -> Int
lastDigit x = x `mod` 10

tens :: Int -> Int
tens x = x `div` 10