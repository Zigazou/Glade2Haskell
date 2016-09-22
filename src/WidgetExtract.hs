{- |
Module      : WidgetExtract
Description : Functions extracting widgets from an XML structure
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

From an XML structure, retrieves every widget.
-}
module WidgetExtract (extractWidgets) where

import Text.XML.Light ( Content (Elem), QName (QName, qName)
                      , Element (elName, elContent), onlyElems, findAttr
                      , findChildren
                      )
import Data.Maybe (catMaybes)
import Widget (Widget (Widget), Signal (Signal))


{- |
Given an XML structure (a list of Content), returns a list of Widget. In a
Glade file, the widgets are identified by the `object` tags.
-}
extractWidgets :: [Content] -> [Widget]
extractWidgets = catMaybes
               . fmap mWidget 
               . filter (("object" ==) . qName . elName)
               . onlyElems
               . flatten getChildren

{- |
Given an Element, Maybe returns a Widget (it needs to have `class` and `id`
attributes.
-}
mWidget :: Element -> Maybe Widget
mWidget element = Widget <$> findAttr (QName "class" Nothing Nothing) element
                         <*> findAttr (QName "id" Nothing Nothing) element
                         <*> (pure . catMaybes) signals
    where signals = mSignal <$> findChildren (QName "signal" Nothing Nothing)
                                element

{- |
Given an Element, Maybe returns a Signal (it needs to have `name` and `handler`
attributes.
-}
mSignal :: Element -> Maybe Signal
mSignal element = Signal <$> findAttr (QName "name" Nothing Nothing) element
                         <*> findAttr (QName "handler" Nothing Nothing) element

{- |
Return the children of a Content. Only Elem Content can have children.
-}
getChildren :: Content -> [Content]
getChildren (Elem element) = elContent element
getChildren _ = []

{- |
A generic function that can flatten any tree given a function that returns
the children of an element.
-}
flatten :: (a -> [a]) -> [a] -> [a]
flatten _ [] = []
flatten f (item:items) = item : flatten f (f item) ++ flatten f items
