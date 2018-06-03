{-# LANGUAGE
    FlexibleInstances
  , OverloadedStrings
  , GADTs
  , RankNTypes
  , UndecidableInstances
  #-}
{-|
Module      : PLPrinter
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A NIH Pretty-Printer
-}
module PLPrinter
  (-- * Types
    Doc ()
  , DocFmt ()

   -- * Render a Doc
  , mkDocFmt
  , docFmt
  , render
  , renderWith

   -- * Create Docs
   -- ** From basic text
  , char
  , text
  , rawText
  , string

  , usingShow

   -- ** Indentation
  , indent
  , indent1

   -- ** From primitive types
  , int
  , bool

   -- **
  , between
  , emptyDoc

  , lineBreak
  , newLine

   -- * Class of things which have a canonical Doc
  , Document
  , document
  , renderDocument

  , Printer (..)
  , emptyPrinter
  , altPrinter
  , purePrinter
  , anyCharPrinter
  , pprint
  , anyTextPrinter
  , rapPrinter

  , bulleted
  )
  where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text

import PLPrinter.Doc

newtype Printer a = Printer {_unPPrint :: a -> Maybe Doc}

pprint :: Printer a -> a -> Maybe Doc
pprint = _unPPrint

rapPrinter :: Printer a -> Printer b -> Printer (a,b)
rapPrinter (Printer p) (Printer q) = Printer $ \(a,b) -> liftM2 mappend (p a) (q b)

-- These functions cant form an Alternative because we can't implement
-- the superclass Functor as we're a contravariant functor.
emptyPrinter :: Printer a
emptyPrinter = Printer . const $ Nothing

altPrinter :: Printer a -> Printer a -> Printer a
altPrinter (Printer p) (Printer q) = Printer $ \a -> mplus (p a) (q a)

purePrinter :: Eq a => a -> Printer a
purePrinter a = Printer $ \a' -> if a == a' then Just mempty else Nothing

anyCharPrinter :: Printer Char
anyCharPrinter = Printer $ Just . char

anyTextPrinter :: Printer Text
anyTextPrinter = Printer $ Just . text

