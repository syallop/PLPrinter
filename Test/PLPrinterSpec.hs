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

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Coerce

spec :: Spec
spec = describe "PLPrinter" $ sequence_
  [
  ]
