{- |
Module      : Widget2Haskell
Description : Transforms a list of Widget into Haskell source code
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Transforms a list of Widget into Haskell source code.
-}
module Widget2Haskell (widgets2Haskell) where

import Widget (Widget (Widget))

{- |
`widgets2Haskell` is a simple templating process.
-}
widgets2Haskell :: String -> [Widget] -> [String]
widgets2Haskell typeName widgets =
    [ "module " ++ typeName ++ " where"
    , ""
    , "import Graphics.UI.Gtk"
    , ""
    , "data " ++ typeName ++ " = " ++ typeName
    ] ++
    indent "    { " "    , " (generateField <$> widgets) ++
    [ "    }"
    , ""
    , "load" ++ typeName ++ " :: String -> IO " ++ typeName
    , "load" ++ typeName ++ " guiPath = do"
    , "    bdr <- builderNew"
    , "    builderAddFromFile bdr guiPath"
    , "    " ++ typeName
    ] ++
    indent "            <$> " "            <*> " (generateCast <$> widgets)

{- |
An auxiliary function returning the string for lines defining the fields of
the record.
-}
generateField :: Widget -> String
generateField (Widget cla ide) = ide ++ " :: " ++ gtkClass cla

{- |
An auxiliary function returning the cast lines of a long applicative call.
-}
generateCast :: Widget -> String
generateCast (Widget cla ide) =
    "builderGetObject bdr castTo" ++ gtkClass cla ++ " \"" ++ ide ++ "\""

{- |
Indents a list of lines given 2 strings: the first is inserted before the first
line, the second is inserted before every other line.
-}
indent :: String -> String -> [String] -> [String]
indent _ _ [] = []
indent firstIndent _ (firstRow:[]) = [ firstIndent ++ firstRow ]
indent firstIndent tailIndent (firstRow:tailRows) =
    (firstIndent ++ firstRow) : fmap (tailIndent ++) tailRows

{- |
Converts a Glade GTK class into an Haskell GTK class. It generally consists of
removing the Gtk prefix.
-}
gtkClass :: String -> String
gtkClass "GtkComboBoxText" = "ComboBox"
gtkClass ('G':'t':'k':className) = className
gtkClass className = className
