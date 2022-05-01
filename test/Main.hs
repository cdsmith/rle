module Main where

import CompressedSeq (CompressedSeq)
import qualified CompressedSeq
import Interval
import RLE
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "RLE" $ do
    it "works" $ do
      let xs = CompressedSeq.fromList "aaaabccaadeeee" :: CompressedSeq RLE Char
      length xs `shouldBe` 14
      length (CompressedSeq.atoms xs) `shouldBe` 6

  describe "Interval" $ do
    it "works" $ do
      let xs =
            CompressedSeq.fromList "abcdefZhijkl" :: CompressedSeq Interval Char
      length xs `shouldBe` 12
      length (CompressedSeq.atoms xs) `shouldBe` 3
