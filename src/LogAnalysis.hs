{-# OPTIONS_GHC -Wall #-}

module LogAnalysis
  ( parse,
    parseMessage,
    inOrder,
    build,
    insert,
    whatWentWrong
  )
  where

import LogAnalysis.Log
import LogAnalysis.Internal



parse :: String -> [LogMessage]
parse chunk = [parseMessage x | x <- lines chunk]

parseMessage :: String -> LogMessage
parseMessage x = createLogMessage (words x)



inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l root r) = (inOrder l) ++ [root] ++ (inOrder r)

build :: [LogMessage] -> MessageTree
build msgs = foldl insert Leaf msgs

insert :: MessageTree -> LogMessage -> MessageTree
insert tree (Unknown _) = tree
insert Leaf new = Node Leaf new Leaf
insert (Node l msg r) new
  | (getStamp msg) > (getStamp new) = Node (insert l new) msg r
  | otherwise                       = Node l msg (insert r new)



whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map errorDescOverFifty (filter isErrorMsg msgs)