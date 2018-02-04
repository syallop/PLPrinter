{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
  #-}
{-|
Module      : PLPrinterSpec
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PLPrinter.
-}
module PLPrinterSpec where

import PLPrinter
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as Text
import Control.Applicative
import Control.Monad

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Coerce

spec :: Spec
spec = do
  describe "PLPrinter" $ sequence_
    [
    ]
  describe "PLPrinter.Doc" $ sequence_
    [
     {-describe "when using Monoid functionality" $ sequence_-}
      {-[it "does not change the doc when mappending mempty before" pending-}
      {-,it "does not change the doc when mappending mempty after" pending-}
      {-,it "produces the same doc (d0 <> d1) <> d2 vs d0 <> (d1 <> d2)" pending-}
      {-]-}

     describe "when a line length m is given" $ sequence_
      [it "no line is longer than m when the document contains no newlines" pending
      ,it "no line is longer than m when the document contains newlines" pending
      ]

    {-,describe "when an indent of n is given" $ sequence_-}
      {-[prop "all lines have n spaces as a prefix" $ pending-}
      {-,it "all lines break at linelength - n" pending-}
      {-]-}

    ,describe "when two indents n and m are given" $ sequence_
      [it "when both indents are at the root, all lines have n+m spaces as prefix" pending
      ,it "lines under n but not m have only n spaces" pending
      ]

    ,describe "when a newline is requested" $ sequence_
      [it "the internal position within the current line becomes 0" pending
      ,it "the number of indent characters is inserted into the next line" pending
      ,it "only one newline is inserted if at the end of the max line length" pending
      ]

    ,describe "when appending" $ sequence_
      [it "documents that expand past the line length are broken at the line length" pending
      ]

    ,describe "when rendering" $ sequence_
      [it "appended documents have length equal or greater than their sum" pending
      ,it "appended text with no newlines has length equal to length plus the number of newlines and spaces that should have been inserted" pending
      ,it "the empty document is the empty string" pending
      ,it "any number of line breaks is still that number of line breaks" pending
      ]
    ]


