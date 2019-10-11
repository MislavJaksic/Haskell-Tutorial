{-# OPTIONS_GHC -Wall #-}

module FER.Course.Lecture01
  ( concat3,
    showSalaryNegativeError,
    showSalary
  )
  where

-- Exercise 1

concat3 :: String -> String -> String -> String
concat3 x y z = if length y > 2 then x ++ y ++ z else x ++ z

showSalaryNegativeError :: String
showSalaryNegativeError = "Error: salary amount is negative"

showSalary :: (Num a, Ord a, Show a) => a -> a -> String
showSalary amount bonus = if amount >= 0 then "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus else showSalaryNegativeError
