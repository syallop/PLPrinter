{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Control.Monad

import PLPrinter
import Data.Text (Text)
import Data.List
import Data.Monoid
import Data.Function
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

-- Parameters for generating Documents
data GenDocParams = GenDocParams
  {_maxLineLength :: Int
  ,_indentNumber  :: Int
  ,_numberOfLines :: Int
  ,_wrapAt        :: Int
  }
  deriving Show

-- Generate parameters for generating documents.
genDocParams :: HHog.Gen GenDocParams
genDocParams = GenDocParams
  <$> (Gen.integral $ Range.linear 0 10)
  <*> (Gen.integral $ Range.linear 1 2)
  <*> (Gen.integral $ Range.linear 1 5)
  <*> (Gen.integral $ Range.linear 10 15)

-- Generate a list of input lines of text to be eventually fed into a Doc.
genDocLines :: GenDocParams -> HHog.Gen [Text]
genDocLines (GenDocParams maxLineLength _indentNumber numberOfLines _wrapAt) =
  replicateM numberOfLines (textLessThan maxLineLength)

-- Generate a document, returning the input lines used to create it.
-- The input lines are interspersed with newlines and all wrapped under an
-- indentation block.
mkIndentedDocument :: GenDocParams -> HHog.Gen ([Text],Doc)
mkIndentedDocument params@(GenDocParams maxLineLength indentNumber numberOfLines _wrapAt) = do
  lines <- genDocLines params
  pure (lines,indent indentNumber . mconcat . intersperse lineBreak . map text $ lines)

-- Render a document with some generated parameters.
renderOutputText :: GenDocParams -> Doc -> Text
renderOutputText (GenDocParams _maxLineLength _indentNumber _numberOfLines wrapAt) doc =
  renderWith (mkDocFmt wrapAt) doc

prop_indentation :: HHog.Property
prop_indentation = HHog.property $ do
  params@(GenDocParams maxLineLength indentNumber numberOfLines wrapAt)
    <- HHog.forAll genDocParams

  (inputLines,doc)
    <- HHog.forAll $ mkIndentedDocument params
  annotateShow inputLines
  annotateShow doc

  let outputText :: Text
      outputText = renderOutputText params doc

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


prop_monoid :: HHog.Property
prop_monoid = HHog.property $ do
  params@(GenDocParams maxLineLength indentNumber numberOfLines wrapAt)
    <- HHog.forAll genDocParams

  -- Create a document
  (inputLinesA,docA) <- HHog.forAll $ mkIndentedDocument params

  -- Appending mempty before does nothing.
  assert $ on (==) (renderOutputText params) (mempty<>docA) docA

  -- Appending mempty after does nothing.
  assert $ on (==) (renderOutputText params) (docA<>mempty) docA

  -- Create two more documents
  (inputLinesB,docB) <- HHog.forAll $ mkIndentedDocument params
  (inputLinesC,docC) <- HHog.forAll $ mkIndentedDocument params

  -- The order of appending is irrelevant
  -- (a<>b)<>c == a<>(b<>c)
  let left  = (docA <> docB) <> docC
      right = docA <> (docB <> docC)
  annotateShow left
  annotateShow right
  assert $ on (==) (renderOutputText params) left right

  -- Appending a newline and then a document after an indented document leaves
  -- the trailing document unindented.
  (inputLinesUnindented,docUnindented)
    <- HHog.forAll $ mkIndentedDocument (GenDocParams maxLineLength 0 2 wrapAt)
  let indentedAndNot      = docA <> lineBreak <> docUnindented
      indentedAndNotText  = renderOutputText params indentedAndNot
      indentedAndNotLines = Text.lines indentedAndNotText
      trailingUnindentedLines = drop numberOfLines indentedAndNotLines
  annotateShow indentedAndNotText
  annotateShow indentedAndNotLines
  assert $ all (\(rendered,input) -> rendered == input) . zip inputLinesUnindented $ trailingUnindentedLines

{- Test possibilities:

- when a line length m is given
  - no line is longer than m when the document contains no newlines
  - no line is longer than m when the document contains newlines

- when two indents n and m are given
  - when both indents are at the root- all lines have n+m spaces as prefix
  - lines under n but not m have only n spaces

- when a newline is requested
  - the internal position within the current line becomes 0
  - the number of indent characters is inserted into the next line
  - only one newline is inserted if at the end of the max line length

- when ap
  - documents that expand past the line length are broken at the line length

- when rendering
  - appended documents have length equal or greater than their sum
  - appended text with no newlines has length equal to length plus the number of newlines and spaces that should have been inserted
  - the empty document is the empty string
  - any number of line breaks is still that number of line breaks

-}
