{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Control.Monad

import PLPrinter
import Data.Text (Text)
import Data.List
import qualified Data.Text as Text

import Hedgehog as HHog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Gen.QuickCheck as Gen
import Test.QuickCheck.Hedgehog as HHog

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence
    [tests
    ]
  pure ()

tests :: IO Bool
tests = checkParallel $$(discover)

textLessThan :: Int -> HHog.Gen Text
textLessThan i
  | i < 0     = pure ""
  | otherwise = Gen.text (Range.linear 1 i) Gen.alphaNum

prop_indentation :: HHog.Property
prop_indentation = HHog.property $ do
  maxLineLength <- HHog.forAll $ Gen.integral $ Range.linear 0 100
  indentNumber  <- HHog.forAll $ Gen.integral $ Range.linear 1 5
  numberOfLines <- HHog.forAll $ Gen.integral $ Range.linear 1 10
  wrapAt        <- HHog.forAll $ Gen.integral $ Range.linear 80 100

  lines         <- HHog.forAll $ replicateM numberOfLines (textLessThan maxLineLength)
  let doc :: Doc
      doc = indent indentNumber . mconcat . intersperse lineBreak . map text $ lines

      outputText :: Text
      outputText = renderWith (mkDocFmt wrapAt) doc

      outputLines :: [Text]
      outputLines = Text.lines outputText
  annotateShow outputText
  annotateShow outputLines

  -- There are always less than or equal input lines than output.
  assert $ numberOfLines <= length outputLines

  -- Each output line is less than the max line length
  assert $ all (\outputLine -> Text.length outputLine <= wrapAt) outputLines

  -- Each output line starts with the desired indent
  let indentation = Text.replicate indentNumber " "
  assert $ all (\outputLine -> Text.take indentNumber outputLine == indentation) outputLines

