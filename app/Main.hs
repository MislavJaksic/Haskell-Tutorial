module Main
  where

import Luhn
import System.Environment



main :: IO ()
main = do
       (input:args) <- getArgs
       let number = read input :: Int
       putStrLn $ show $ validate number
