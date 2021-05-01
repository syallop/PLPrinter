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

This package provides a pretty-printer and is abstracted from an effort to build
a programming language with minimal dependencies.

This module exports 'Printers' - functions which export 'Doc'uments which may fail.
Printers can be composed where each or either Printer must succeed.

A design goal is to represent them as a dual to 'Parsers' such that syntax can
be defined that maps to both a Printer and a Parser that are the inverse of each
other.

See 'PLPrinter.Doc' for functions which consume and render Documents.
See 'README.md' for an overview of the API.
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

import Control.Monad
import Data.Text (Text)

import PLPrinter.Doc

-- | A Printer attempts to transform values 'a' into 'Doc'uments.
-- It can be thought of as having type 'a -> Maybe Doc'.
newtype Printer a = Printer {_unPPrint :: a -> Maybe Doc}

-- | Run a 'Printer' against some input. A successfully printed document or a
-- failure is returned.
pprint :: Printer a -> a -> Maybe Doc
pprint = _unPPrint

-- | Directly print any character.
anyCharPrinter :: Printer Char
anyCharPrinter = Printer $ Just . char

-- | Directly print any text.
anyTextPrinter :: Printer Text
anyTextPrinter = Printer $ Just . text

-- | The empty printer always fails.
emptyPrinter :: Printer a
emptyPrinter = Printer . const $ Nothing

-- | The pure printer succeeds when the value is equal to the one provided.
purePrinter :: Eq a => a -> Printer a
purePrinter a = Printer $ \a' -> if a == a' then Just mempty else Nothing

-- | Two alternative printers can be tried left to right and succeeds if either
-- succeeds.
altPrinter :: Printer a -> Printer a -> Printer a
altPrinter (Printer p) (Printer q) = Printer $ \a -> mplus (p a) (q a)

-- | Compose two printers that must both succeed.
-- This is similar to 'ap' but we must use tuples here rather than the standard
-- applicative formation. This is because we cannot create a sensible 'Doc'
-- given the type 'Printer (a -> b)'.
rapPrinter :: Printer a -> Printer b -> Printer (a,b)
rapPrinter (Printer p) (Printer q) = Printer $ \(a,b) -> liftM2 mappend (p a) (q b)




