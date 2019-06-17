module HanoiSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Hanoi



main :: IO ()
main = hspec spec



spec :: Spec
spec = do
  describe "hanoi" $ do
    it "list tower of hanoi moves for 3 disks" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
      
    it "ist tower of hanoi moves for 5 disks" $ do
      hanoi 5 "a" "b" "c" `shouldBe` [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b"),("a","c"),("b","c"),("b","a"),("c","a"),("b","c"),("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b"),("c","a"),("b","c"),("b","a"),("c","a"),("c","b"),("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]