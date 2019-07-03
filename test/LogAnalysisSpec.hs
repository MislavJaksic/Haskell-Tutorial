module LogAnalysisSpec
  ( main
  , spec
  ) where

import Test.Hspec

import LogAnalysis
import LogAnalysis.Log
import LogAnalysis.Internal



main :: IO ()
main = hspec spec



spec :: Spec
spec = do
  describe "parse" $ do
    it "parse messages" $ do
      parse "E 2 562 help help\nI 29 la la la\nThis is not in the right format" `shouldBe` [LogMessage (Error 2) 562 "help help", LogMessage Info 29 "la la la", Unknown "This is not in the right format"]
  
  describe "parseMessage" $ do
    it "parse an error message" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      
    it "parse an info message" $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      
    it "parse an unknown input" $ do
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
      
      
      
  describe "createLogMessage" $ do
    it "parse an error message" $ do
      createLogMessage ["E", "2", "562", "help", "help"] `shouldBe` LogMessage (Error 2) 562 "help help"
      
    it "parse an info message" $ do
      createLogMessage ["I", "29", "la", "la", "la"] `shouldBe` LogMessage Info 29 "la la la"
      
    it "parse an unknown input" $ do
      createLogMessage ["This", "is", "not", "in", "the", "right", "format"] `shouldBe` Unknown "This is not in the right format"
      
  describe "toInt" $ do
    it "string to integer" $ do
      toInt "1 " `shouldBe` 1
      
      
      
  describe "getMsgType" $ do
    it "get LogMessage message type" $ do
      getMsgType (LogMessage Info 29 "la la la") `shouldBe` Info
      
  describe "getStamp" $ do
    it "get LogMessage timestamp" $ do
      getStamp (LogMessage Info 29 "la la la") `shouldBe` 29
      
    it "get Unknown timestamp" $ do
      getStamp (Unknown "1") `shouldBe` -1
      
  describe "getDesc" $ do
    it "get LogMessage description" $ do
      getDesc (LogMessage Info 29 "la la la") `shouldBe` "la la la"
      
    it "get Unknown description" $ do
      getDesc (Unknown "1") `shouldBe` "1"
      
      
      
  describe "getSeverity" $ do
    it "get Error severity" $ do
      getSeverity (Error 5) `shouldBe` 5
      
    it "Warning is not an error" $ do
      getSeverity Warning `shouldBe` -1
      
    it "Info is not an error" $ do
      getSeverity Info `shouldBe` -1
      
  
  
  describe "errorDescOverFifty" $ do
    it "in not an error message" $ do
      errorDescOverFifty (LogMessage Info 0 "a") `shouldBe` ""
      
    it "in not an error message" $ do
      errorDescOverFifty (Unknown "a") `shouldBe` ""
    
    it "get error description if error severity is 50 or more" $ do
      errorDescOverFifty (LogMessage (Error 99) 562 "help help") `shouldBe` "help help"
     
     
      
  describe "isErrorMsg" $ do
    it "not an error message" $ do
      isErrorMsg (Unknown "This is not in the right format") `shouldBe` False
      
    it "is a message but not an error message" $ do
      isErrorMsg (LogMessage Info 29 "la la la") `shouldBe` False
      
    it "is an error message" $ do
      isErrorMsg (LogMessage (Error 2) 562 "help help") `shouldBe` True
  
  
  
  describe "inOrder" $ do
    it "get in order messages" $ do
      inOrder (Node (Node (Node Leaf (LogMessage Info 0 "a") Leaf) (LogMessage Info 1 "a") Leaf) (LogMessage Info 2 "a") Leaf) `shouldBe` [(LogMessage Info 0 "a"), (LogMessage Info 1 "a"), (LogMessage Info 2 "a")] 
  
  describe "build" $ do
    it "build a tree of LogMessages" $ do
      build [(LogMessage Info 2 "a"), (LogMessage Info 1 "a"), (LogMessage Info 0 "a")] `shouldBe` Node (Node (Node Leaf (LogMessage Info 0 "a") Leaf) (LogMessage Info 1 "a") Leaf) (LogMessage Info 2 "a") Leaf
      
  describe "insert" $ do
    it "insert Unknown and return the same tree" $ do
      insert Leaf (Unknown "1") `shouldBe` Leaf
      
    it "insert Unknown and return the same tree" $ do
      insert (Node Leaf (LogMessage Info 1 "a") Leaf) (Unknown "1") `shouldBe` (Node Leaf (LogMessage Info 1 "a") Leaf)
      
    it "insert LogMessage into an empty tree" $ do
      insert Leaf (LogMessage Info 1 "a") `shouldBe` (Node Leaf (LogMessage Info 1 "a") Leaf)
      
    it "insert LogMessage into the right sub-tree" $ do
      insert (Node Leaf (LogMessage Info 1 "a") Leaf) (LogMessage Info 2 "a") `shouldBe` (Node Leaf (LogMessage Info 1 "a") (Node Leaf (LogMessage Info 2 "a") Leaf))
      
    it "insert LogMessage into the left sub-tree" $ do
      insert (Node Leaf (LogMessage Info 1 "a") Leaf) (LogMessage Info 0 "a") `shouldBe` (Node (Node Leaf (LogMessage Info 0 "a") Leaf) (LogMessage Info 1 "a") Leaf)
      
      
      
  describe "whatWentWrong" $ do
    it "get descriptions of mesages with error severities of 50 or more" $ do
      whatWentWrong [(LogMessage Info 0 "a"), (LogMessage Warning 0 "a"), (Unknown "a"), (LogMessage (Error 3) 0 "ab"),  (LogMessage (Error 99) 0 "abcd")] `shouldBe` ["", "abcd"] 
