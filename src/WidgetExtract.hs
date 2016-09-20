module WidgetExtract (extractWidgets) where

import Text.XML.Light ( Content (Elem), QName (QName, qName)
                      , Element (elName, elContent), onlyElems, findAttr
                      )
import Data.Maybe (catMaybes)
import Widget (Widget (Widget))

extractWidgets :: [Content] -> [Widget]
extractWidgets = catMaybes
               . fmap mWidget 
               . filter (("object" ==) . qName . elName)
               . onlyElems
               . flatten getChildren

mWidget :: Element -> Maybe Widget
mWidget element = Widget <$> findAttr (QName "class" Nothing Nothing) element
                         <*> findAttr (QName "id" Nothing Nothing) element

getChildren :: Content -> [Content]
getChildren (Elem element) = elContent element
getChildren _ = []

flatten :: (a -> [a]) -> [a] -> [a]
flatten _ [] = []
flatten f (item:items) = item : flatten f (f item) ++ flatten f items
