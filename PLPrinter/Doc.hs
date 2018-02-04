{-# LANGUAGE
    FlexibleInstances
  , OverloadedStrings
  , RankNTypes
  , UndecidableInstances
  #-}
{-|
Module      : PLPrinter.Doc
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A NIH Pretty-Printer
-}
module PLPrinter.Doc
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
  , string

  , usingShow

  -- ** Indentation
  , indent
  , indent1

  -- ** From primitive types
  , int
  , bool

  -- **
  , emptyDoc

  , lineBreak
  , newLine

  -- * Class of things which have a canonical Doc
  , Document(..)
  , renderDocument

  , parens
  , between
  )
  where

import Data.List
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad
import Control.Applicative
import Data.String

-- | A 'Doc'ument is something that can be printed nicely and appended to
-- efficiently.
data Doc
  = DocText Text      -- ^ Literal text
  | DocIndent Int Doc -- ^ Indent a Doc by an additional amount
  | DocAppend Doc Doc -- ^ Append two documents
  | DocEmpty          -- ^ Empty document
  | DocBreak          -- ^ Line break
  deriving Eq

data DocFmt = DocFmt
  {_lineLength  :: Int -- maximum total length of a line of text before a new line is inserted
  ,_indent      :: Int -- number of spaces inserted at the start of a line of text
  ,_colPosition :: Int -- the current position within a line. Always greater than the indent and less than the line length.
                       -- Used when appending to remember how far into the current line we are
  }

instance Monoid Doc where
  mempty  = DocEmpty

  mappend DocEmpty y        = y
  mappend x        DocEmpty = x
  mappend x        y        = DocAppend x y

docFmt :: DocFmt
docFmt = DocFmt 80 0 0

mkDocFmt :: Int -> DocFmt
mkDocFmt lineLength = DocFmt lineLength 0 0

-- | Render a document to Text with some 'DocFmt' formatting settings.
renderWith :: DocFmt -> Doc -> Text
renderWith fmt doc = fst $ renderWith' fmt doc
  where
    renderWith' :: DocFmt -> Doc -> (Text,DocFmt)
    renderWith' fmt d = case d of
      DocText inputTxt
        -> let indentation :: Text
               indentation = Text.replicate (_indent fmt) " "

               txt :: Text
               txt = inputTxt

               requestedCharacters :: Int
               requestedCharacters = maximumUsableLength fmt

               line :: Text
               rest :: Text
               (line,rest) = Text.splitAt requestedCharacters txt

               takenCharacters :: Int
               takenCharacters = Text.length line

            in if takenCharacters < requestedCharacters
                 -- Took the last characters. End.
                 then (mconcat [indentation,line],fmt{_colPosition = _colPosition fmt + takenCharacters})

                 -- Took a lines worth, recurse.
                 else let renderedLine = mconcat [indentation,line,"\n"]
                          (rest,fmt)   = renderWith' (fmt{_colPosition=0}) (DocText rest)
                         in (renderedLine<>rest,fmt)

      DocIndent i d
        -> let newIndent   = _indent fmt + i
               newFmt      = fmt{_indent = newIndent
                                }
             in renderWith' newFmt d

      DocEmpty
        -> (Text.empty,fmt)

      -- Append two Docs with either a space between them or a break to a new line
      DocAppend d0 d1
        -> let (txt0,fmt0) = renderWith' fmt  d0
               (txt1,fmt1) = renderWith' fmt0 d1
              in (txt0<>txt1,fmt1)

      DocBreak
        -> ("\n",fmt{_colPosition = 0})

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- The amount of usable characters in a line.
-- The maximum lines length subtract the number of spaces in the indentation.
maximumUsableLength :: DocFmt -> Int
maximumUsableLength fmt = _lineLength fmt - _colPosition fmt

-- A string of spaces for the current indentation level
indentSpaces :: DocFmt -> Text
indentSpaces fmt = Text.replicate (_indent fmt) " "

-- | Render a 'Doc'ument to Text with default formatting settings.
render :: Doc -> Text
render = renderWith docFmt

-- Print a single char
char :: Char -> Doc
char = text . Text.singleton

-- Print some text.
text :: Text -> Doc
text = DocText . Text.filter (/= '\n')

string :: String -> Doc
string = text . Text.pack

-- Use a things show instance as input to DocText
usingShow :: Show a => a -> Doc
usingShow = string . show

bool :: Bool -> Doc
bool = usingShow

int :: Int -> Doc
int = usingShow

-- Indent a document by a quantity
indent :: Int -> Doc -> Doc
indent = DocIndent

-- Indent a document by a single space
indent1 :: Doc -> Doc
indent1 = DocIndent 1

lineBreak :: Doc
lineBreak = DocBreak

newLine :: Doc -> Doc
newLine d = d <> lineBreak

emptyDoc :: Doc
emptyDoc = DocEmpty

class Document d where
  document :: d -> Doc

renderDocument :: Document d => d -> Text
renderDocument = render . document

instance Document ()     where document () = text "()"
instance Document Char   where document = char
instance Document Text   where document = text
instance Document String where document = string
instance Document Bool   where document = bool
instance Document Int    where document = int
instance Document Doc    where document = id
{-instance Document [Doc]  where document = foldr DocAppend DocEmpty-}

{-instance IsString Doc where-}
  {-fromString = string-}

between :: Doc -> Doc -> Doc -> Doc
between l a r = l <> a <> r

parens :: Doc -> Doc
parens a = between (char '(') a (char ')')

