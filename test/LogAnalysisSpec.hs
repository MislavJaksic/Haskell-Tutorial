module LogAnalysisSpec
  ( main
  , spec
  ) where

import Test.Hspec

import LogAnalysis
import LogAnalysis.Internal



main :: IO ()
main = hspec spec



spec :: Spec
spec = do
  describe "splitMessage" $ do
    it "split error message string" $ do
      splitMessage "E 2 562 help help" `shouldBe` ["E", "2", "562", "help", "help"]
      
    it "split info message string" $ do
      splitMessage "I 29 la la la" `shouldBe` ["I", "29", "la", "la", "la"]
      
    it "split incorrect message string" $ do
      splitMessage "This is not in the right format" `shouldBe` ["This", "is", "not", "in", "the", "right", "format"]
      
  -- describe "parseMessage" $ do
    -- it "" $ do
      -- parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      
    -- it "" $ do
      -- parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      
    -- it "" $ do
      -- parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
