{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CompressedSeq (CompressedSeq)
import qualified CompressedSeq
import Compression (Compression)
import Control.Monad (forM_, when)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Interval (Interval (Interval))
import RLE (RLE (..))
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

data Confluence = TestConfluence | Don'tTestConfluence deriving (Eq)

ifTestingConfluence :: Applicative f => Confluence -> f () -> f ()
ifTestingConfluence conf = when (conf == TestConfluence)

compressedSeqProps ::
  forall f a.
  (Compression f a, Show a, Eq a, Show (f a), Eq (f a)) =>
  Confluence ->
  CompressedSeq f a ->
  SpecWith ()
compressedSeqProps conf xs = do
  it "can be split and reconcat" $ do
    forM_ [0 .. length xs] $ \i -> do
      let (a, b) = CompressedSeq.splitAt i xs
      length a `shouldBe` i

      toList (a <> b) `shouldBe` toList xs
      ifTestingConfluence conf $ a <> b `shouldBe` xs

  it "has associative concatenation" $ do
    forM_ [0 .. length xs] $ \j -> do
      forM_ [0 .. j] $ \i -> do
        -- Consider all splits of the list
        let (ab, c) = CompressedSeq.splitAt j xs
        let (a, b) = CompressedSeq.splitAt i ab

        -- The associative property should hold.
        toList (a <> (b <> c)) `shouldBe` toList ((a <> b) <> c)
        ifTestingConfluence conf $ a <> (b <> c) `shouldBe` (a <> b) <> c

        -- Passing through uncompressed lists shouldn't change anything.
        let a' = CompressedSeq.fromList (toList a) :: CompressedSeq f a
        let b' = CompressedSeq.fromList (toList b)
        let c' = CompressedSeq.fromList (toList c)
        (a' <> b') <> c' `shouldBe` a' <> (b' <> c')

main :: IO ()
main = hspec $ do
  describe "Identity Compression" $ do
    let xs = CompressedSeq.fromList "abcde" :: CompressedSeq Identity Char
    it "compresses into expected atoms" $ do
      toList xs `shouldBe` "abcde"
      CompressedSeq.atoms xs
        `shouldBe` [ Identity 'a',
                     Identity 'b',
                     Identity 'c',
                     Identity 'd',
                     Identity 'e'
                   ]
    compressedSeqProps TestConfluence xs

  describe "RLE" $ do
    let xs = CompressedSeq.fromList "aaaabccaadeeee" :: CompressedSeq RLE Char
    it "compresses into expected atoms" $ do
      toList xs `shouldBe` "aaaabccaadeeee"
      CompressedSeq.atoms xs
        `shouldBe` [ Run 'a' 4,
                     Run 'b' 1,
                     Run 'c' 2,
                     Run 'a' 2,
                     Run 'd' 1,
                     Run 'e' 4
                   ]
    compressedSeqProps TestConfluence xs

  describe "Interval" $ do
    let xs =
          CompressedSeq.fromList "abcdefZhijkl" :: CompressedSeq Interval Char
    it "compresses into expected atoms" $ do
      toList xs `shouldBe` "abcdefZhijkl"
      CompressedSeq.atoms xs
        `shouldBe` [ Interval (fromEnum 'a') (fromEnum 'f'),
                     Interval (fromEnum 'Z') (fromEnum 'Z'),
                     Interval (fromEnum 'h') (fromEnum 'l')
                   ]
    compressedSeqProps TestConfluence xs

  describe "Compose Compression" $ do
    let xs =
          mconcat . map CompressedSeq.atom $
            [ Compose (Run (Interval (fromEnum 'a') (fromEnum 'c')) 2),
              Compose (Run (Interval (fromEnum 'a') (fromEnum 'd')) 2)
            ]
    it "compresses into expected atoms" $ do
      toList xs `shouldBe` "abcabcabcdabcd"
      CompressedSeq.atoms xs
        `shouldBe` [ Compose (Run (Interval (fromEnum 'a') (fromEnum 'c')) 2),
                     Compose (Run (Interval (fromEnum 'a') (fromEnum 'd')) 2)
                   ]
    compressedSeqProps Don'tTestConfluence xs
