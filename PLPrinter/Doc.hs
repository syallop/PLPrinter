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

'Documents' are defined as things which can be printed nicely and appended to
efficiently. Documents are composed of 'Text', 'line-breaks' and regions of
'indentation' and may be 'appended' together.

Documents can be deconstructed by a 'render' function which takes formatting
parameters to produce Text. The main supported parameter is line length. When
rendering a Document, lines which exceed this length will have a newline
character forcibly inserted.
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
  , emptyDoc

  , lineBreak
  , newLine

  -- * Class of things which have a canonical Doc
  , Document(..)
  , renderDocument

  , parens
  , between
  , bulleted
  )
  where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid
import Data.Semigroup
import Data.String
import Data.String
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import qualified Data.Text as Text

-- | A 'Doc'ument is something that can be printed nicely and appended to
-- efficiently.
data Doc
  -- ^ Literal text which never contains a newline.
  = DocText Text

  -- ^ Append two documents together.
  | DocAppend Doc Doc

  -- ^ Adjust the requested indentation level for a document.
  | DocIndent Int Doc

  -- ^ Insert a line break now. Indentation characters are only inserted when
  -- text is appended.
  | DocBreak

  -- ^ The empty document.
  | DocEmpty
  deriving (Show, Eq, Ord)

-- | Formatting configuration used when rendering a document. Carries maximum
-- line length as well as the current indentation and position within a line.
--
-- Construct with 'mkDocFmt' or use default 'docFmt'.
data DocFmt = DocFmt
  { -- The maximum number of characters allowed on a line before a newline is
    -- forcibly inserted. This includes any indentation and does not include
    -- the newline character.
    _lineLength  :: !Int

    -- The number of spaces to be inserted whenever text is to be written and it
    -- is the start of a new line.
  , _indent      :: !Int

    -- The current position within a line.
    -- Between 0 and the lineLength.
    -- A position of 0 indicates a newline has just been written and so any
    -- following text must indent.
  , _colPosition :: !Int
  }

-- Documents combine via DocAppend with DocEmpty as the empty document.
instance Semigroup Doc where
  DocEmpty <> y        = y
  x        <> DocEmpty = x
  x        <> y        = DocAppend x y

-- DocEmpty is the empty document.
instance Monoid Doc where
  mempty  = DocEmpty

-- | The default document format has a max line length of 250 and begins with no
-- indentation.
docFmt :: DocFmt
docFmt = DocFmt 250 0 0

-- | Create a DocFmt with the given line length. Indentation begins at 0.
mkDocFmt :: Int -> DocFmt
mkDocFmt lineLength = DocFmt lineLength 0 0

-- | Render a document to Text with some 'DocFmt' formatting settings.
renderWith :: DocFmt -> Doc -> Text
renderWith fmt doc = toStrict $ toLazyText $ fst $ renderWith' fmt doc
  where
    renderWith' :: DocFmt -> Doc -> (Builder,DocFmt)
    renderWith' fmt@(DocFmt lineLength indent colPosition) doc = case doc of

      -- DocText never contains a newline character itself.
      -- When the column position is 0 we must add any requested indentation.
      -- When the max line length is hit, we must insert a linebreak.
      DocText inputText
        -> let indentationText :: Text
               indentationText = if colPosition == 0 then Text.replicate indent " " else ""

               remainingSpace :: Int
               remainingSpace = lineLength - Text.length indentationText

               atMostALine :: Text
               restText    :: Text
               (atMostALine,restText) = Text.splitAt remainingSpace inputText

               lineSoFar :: Builder
               lineSoFar = fromText indentationText <> fromText atMostALine

               -- Did we manage to take a while line worth of text>
               tookAWholeLine :: Bool
               tookAWholeLine = Text.length atMostALine == remainingSpace
             in if not tookAWholeLine
                   -- If we didnt take a whole line, then the text must be
                   -- finished. Our new position is the length we took plus the
                   -- indentation we added.
                   then (lineSoFar, fmt{_colPosition = colPosition + Text.length indentationText + Text.length atMostALine})

                   -- A whole line was taken
                   else if restText == ""
                          -- Because it fit _exactly_.
                          -- Add a new line, reset the column position and we're
                          -- done with the text.
                          then (lineSoFar <> "\n", fmt{_colPosition = 0})

                          -- And there is more text to go.
                          -- Add a new line, reset the column position and
                          -- recurse with the remaining DocText.
                          else let thisLine :: Builder
                                   thisLine = lineSoFar <> "\n"

                                   nextFmt :: DocFmt
                                   nextFmt = fmt{_colPosition = 0}

                                   otherLines :: Builder
                                   finalFmt   :: DocFmt
                                   (otherLines, finalFmt) = renderWith' nextFmt (DocText restText)
                                  in (thisLine <> otherLines, finalFmt)

      -- Documents are appended by rendering one document and then the next.
      DocAppend d0 d1
        -> let (txt0,fmt0) = renderWith' fmt  d0
               (txt1,fmt1) = renderWith' fmt0 d1
              in (txt0<>txt1,fmt1)

      -- Change the requested indent when a text is next inserted on a newline.
      DocIndent indentChange doc
        -> let (txt, fmtAfter) = renderWith' (fmt{_indent = indent + indentChange}) doc
            in (txt, fmtAfter{_indent = indent})

      -- Break by inserting a newline and resetting the position to 0.
      DocBreak
        -> ("\n",fmt{_colPosition = 0})

      -- Change nothing.
      DocEmpty
        -> (mempty,fmt)

-- | Render a 'Doc'ument to Text with default formatting settings.
render :: Doc -> Text
render = renderWith docFmt

-- | Print a single char.
char :: Char -> Doc
char = text . Text.singleton

-- | Print some text which may not contain newline characters.
text :: Text -> Doc
text = DocText . Text.filter (/= '\n')

-- | Print text replacing newlines with an explicit line-break.
rawText :: Text -> Doc
rawText = mconcat . intersperse DocBreak . map DocText . Text.lines

-- | Convert a string to text with newline characters removed.
string :: String -> Doc
string = text . Text.pack

-- | Use a things show instance as input to DocText
usingShow :: Show a => a -> Doc
usingShow = string . show

-- | Convert a Bool to a Doc using its show instance.
bool :: Bool -> Doc
bool = usingShow

-- | Convert an Int to a Doc using its show instance.
int :: Int -> Doc
int = usingShow

-- | Indent a document by a number of spaces.
indent :: Int -> Doc -> Doc
indent = DocIndent

-- | Indent a document by a single space.
indent1 :: Doc -> Doc
indent1 = DocIndent 1

-- | Insert a line break.
lineBreak :: Doc
lineBreak = DocBreak

-- | A document followed by a newline.
newLine :: Doc -> Doc
newLine d = d <> lineBreak

-- | The empty document.
emptyDoc :: Doc
emptyDoc = DocEmpty

-- | Document describes the canonical document associated with a type.
class Document d where
  document :: d -> Doc

-- | Convert a value to its canonical document and render it to text.
renderDocument :: Document d => d -> Text
renderDocument = render . document

instance Document ()     where document () = text "()"
instance Document Char   where document = char
instance Document Text   where document = text
instance Document String where document = string
instance Document Bool   where document = bool
instance Document Int    where document = int
instance Document Doc    where document = id

-- | A left document, a document and a right document is the document between
-- the left and right document.
between :: Doc -> Doc -> Doc -> Doc
between l a r = l <> a <> r

-- | Parens wraps a document in '(' ')' parentheses.
parens :: Doc -> Doc
parens a = between (char '(') a (char ')')

-- | Bulleted inserts '- ' before each document on a new line.
bulleted :: [Doc] -> Doc
bulleted = foldr (\doc docAcc -> docAcc <> lineBreak <> text "- " <> doc) mempty

