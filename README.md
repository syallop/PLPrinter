# PLPrinter - A pretty printer built to be used with an in-progress Programming Language 'PL'.

This package defines 'Doc'uments - things which can be printed nicely and
appended to efficiently.

Documents may be composed of text, line-breaks and regions of indentation and
may be appended together.

Documents can be constructed by Printers - functions which map values to possible Documents.

Documents can be deconstructed by a 'render' function which takes formatting
parameters to produce Text. The main supported parameter is line length. When
rendering a Document, lines which exceed this length will have a newline
character forcibly inserted.

