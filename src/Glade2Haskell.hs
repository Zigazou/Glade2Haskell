{- |
Module      : Glade2Haskell
Description : Transform a Glade file into Haskell source code
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Glade2Haskell reads a Glade file and extracts the widget definitions in order
to create an Haskell source code file which can load a Glade file and retrieve
each widget to fill a record.
-}
module Glade2Haskell (glade2Haskell) where

import Text.XML.Light (parseXML)
import Widget2Haskell (widgets2Haskell)
import WidgetExtract (extractWidgets)

{- |
Given a type name and a file path, it loads a Glade file, parse its XML content
and return a list of String ready to be unlined and output.
-}
glade2Haskell :: String -> String -> IO ([String], [String], [String])
glade2Haskell typeName filepath = do
    xml <- readFile filepath
    return (widgets2Haskell typeName (extractWidgets (parseXML xml)))

