module Glade2Haskell (glade2Haskell) where

import Text.XML.Light (parseXML)
import Widget2Haskell (widgets2Haskell)
import WidgetExtract (extractWidgets)

glade2Haskell :: String -> String -> IO [String]
glade2Haskell typeName filepath = do
    xml <- readFile filepath
    return (widgets2Haskell typeName (extractWidgets (parseXML xml)))

