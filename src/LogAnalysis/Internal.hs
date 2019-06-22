module LogAnalysis.Internal
  ( splitMessage
  )
  where

import LogAnalysis.Log

import Data.List.Split



readFileContents :: (String -> [LogMessage]) -> String -> IO [LogMessage]
readFileContents parse file = do 
                              parse <$> readFile "error.log"



-- parseMessage :: String -> LogMessage
-- parseMessage x = LogMessage x

splitMessage :: String -> [String]
splitMessage x = splitOn " " x