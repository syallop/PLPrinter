# PLPrinter - experimental

This package provides a pretty-printer and is abstracted from an effort to build
a programming language with minimal external dependencies.

`Documents` are defined as things which can be printed nicely and appended to
efficiently. Documents are composed of `Text`, `line-breaks` and regions of
`indentation` and may be `appended` together.

Documents can be constructed by `Printers` which may fail and be composed where
each or either Printer must succeed. A design goal is to represent them as a
dual to `Parsers` such that syntax can be defined that maps to both a Printer and
a Parser that are the inverse of each other.

Documents can be deconstructed by a `render` function which takes formatting
parameters to produce Text. The main supported parameter is line length. When
rendering a Document, lines which exceed this length will have a newline
character forcibly inserted.

## Table of contents
- [Directory overview](#directory-overview)
- [Documents](#documents)
  - [Constructing Documents](#constructing-documents)
    - [Adding text](#adding-text)
    - [Transforming documents](#transforming-documents)
    - [Misc combinators](#misc-combinators)
    - [Canonical transformations](#canonical-transformations)
  - [Rendering Documents](#rendering-documents)
- [Printers](#printers)


## Directory overview

| Module          | Location              | Description                                                                                                                |
| ----            | ----                  | ----                                                                                                                       |
| PLPrinter       | PLPrinter.hs          | Defines 'Printer's - functions which convert values into 'Doc'uments as well as re-exporting the contents of PLPrinter.Doc |
| PLPrinter.Doc   | PLPrinter/Doc.hs      | Declares 'Doc'uments, their construction and deconstruction functions.                                                     |
| Main            | Test/HedgehogTests.hs | A small number of test cases for the rendering of Documents. Run with `stack test`                                         |

## Documents
### Constructing documents
#### Adding text
Build documents from text with:
```Haskell
-- Single characters.
> char 'a'
a

-- Text which may not contain newlines.
> text "bcd"
bcd

-- Plain strings are converted to text.
> string "efg"
efg

-- Ints are converted with their 'Show' instance.
> int 1
1

-- Bools are converted with their 'Show' instance.
> bool False
False

-- Values with show instances can be converted to documents.
> usingShow (True,1)
(True,1)

-- An empty document has no text.
> emptyDoc

```

#### Transforming documents

Append documents:
```Haskell
-- Documents append together across a line.
> char 'a' <> text "bcd" <> string "efg"
abcdefg

-- Empty documents do not effect the output.
> char 'a' <> emptyDoc <> text "bcd" <> emptyDoc <> text "efg"
abcdefg
```

Line breaks:
```Haskell
-- Linebreaks insert a newline.
> text "abc" <> lineBreak <> "def"-- | Two alternative printers can be tried left to right and succeeds if either
-- succeeds.
altPrinter :: Printer a -> Printer a -> Printer a
altPrinter (Printer p) (Printer q) = Printer $ \a -> mplus (p a) (q a)

abc
def
```

Documents may be indented:
```Haskell
-- Add the desired number of spaces of indentation before a document.
> indent 4 $ text "abc"
    abc

-- Indentation can be applied to an appended document.
-- Following documents are un-indented automatically.
> indent 4 (text "abc" <> text "def") <> text "ghi"
    abcdef
ghi

-- Linebreaks inside an indent retain the indentation level for following text.
> indent 4 (text "abc" <> lineBreak <> text "def") <> text "ghi"
    abc
    def
ghi

-- Indentation can be nested.
> indent 4 (text "abc" <> lineBreak <> indent 4 (text "def")) <> text "ghi"
    abc
        def
ghi
```

#### Misc combinators
Some combinators are provided:
```Haskell
-- Place a document between to others
> between (char '(') (char ')') (text "abc")
(abc)

-- Place a document between parentheses
> parens "abc"
(abc)

-- Create a bulleted list of documents
> bulleted [char "1", char "b", text "III.", text "four"]
- 1
- b
- III.
- four
```

#### Canonical transformations

A class 'Document' is provided for the 'canonical' representation of types:
```Haskell
-- Document is defined as:
> class Document d where
>  document :: d -> Doc

-- Document common types:
> document "abc"
abc
> document True
true

-- Define document instances for custom types:
> data Tuple a b = Tuple a b
> instance (Document a, Document b) => Document (Tuple a b) where
>   document (Tuple a b) = document "Tuple:" <> lineBreak <> indent 4 (bulleted . map document $ [a, b])
> document (Tuple True 1)
Tuple:
    - True
    - 1
```

### Rendering Documents
Rendering a document means to transform it into a Textual representation.

Constituent documents are transformed into text, appended and indented and line
broke where necessary. The `renderWith` function takes formatting configuration
which can be used to control how long lines will grow before they have an
automatic line break inserted. This process should respect indentation and
appends in a sensible way. E.G.

```haskell
-- Lines are broken when they meet the given length.
> renderWith (mkDocFmt 2) $ text "123456789"
12
34
56
78
9

-- Indentation is continued on the newline.
> renderWith (mkDocFmt 5) $ indent 2 $ text "123456789"
  123
  456
  789

-- Manual linebreaks behave predictably.
> renderWith (mkDocFmt 5) $ indent 2 $ text "1" <> lineBreak <> text "23456789"
  1
  234
  567
  89
```

## Printers
A 'Printer' attempts to transform values into 'Doc'uments and can be though of as being
typed `a -> Maybe Doc`. Some simple printers are provided.

Textual printers which always succeed:
```Haskell
-- Character printer.
> pprint anyCharPrinter 'a'
Just 'a'

-- Text printer.
> pprint anyTextPrinter "abc"
Just "abc"
```

The empty printer:
```Haskell
-- The empty printer always fails
> pprint emptyPrinter "abc"
Nothing
```

The pure printer:
```Haskell
-- The pure printer succeeds when the value is equal to the one
provided.
> pprint (purePrinter 'a') 'a'
Just 'a'

> pprint (purePrinter 'a') 'b'
Nothing
```

Two alternative printers can be tried left to right:
```Haskell
-- The alternative printer succeeds if either printer succeeds:
> pprint (altPrinter (anyCharPrinter 'a') (anyCharPrinter 'b')) 'a'
Just 'a'

> pprint (altPrinter (anyCharPrinter 'a') (anyCharPrinter 'b')) 'b'
Just 'b'

> pprint (altPrinter (anyCharPrinter 'a') (anyCharPrinter 'b')) 'c'
Nothing
```

We can compose two printers that must both succeed. This is similar to `ap`
but we must use tuples here rather than the standard applicative formation.
This is because we cannot create a sensible `Doc` given the type `Printer (a ->
b):
```Haskell
-- When both printers succeed, so does the print.
> pprint (rapPrinter (anyCharPrinter 'a') (anyTextPrinter "bcd")) ('a',"bcd")
Just ('a',"bcd")

-- The first printer failing fails the print.
> pprint (rapPrinter (anyCharPrinter 'a') (anyTextPrinter "bcd")) ('z',"bcd")
Nothing

-- The second printer failing fails the print.
> pprint (rapPrinter (anyCharPrinter 'a') (anyTextPrinter "bcd")) ('a',"zzz")
Nothing
```


