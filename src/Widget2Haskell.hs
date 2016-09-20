module Widget2Haskell (widgets2Haskell) where

import Widget (Widget (Widget))

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

generateField :: Widget -> String
generateField (Widget cla ide) = ide ++ " :: " ++ gtkClass cla

generateCast :: Widget -> String
generateCast (Widget cla ide) =
    "builderGetObject bdr castTo" ++ gtkClass cla ++ " \"" ++ ide ++ "\""

indent :: String -> String -> [String] -> [String]
indent _ _ [] = []
indent firstIndent _ (firstRow:[]) = [ firstIndent ++ firstRow ]
indent firstIndent tailIndent (firstRow:tailRows) =
    (firstIndent ++ firstRow) : fmap (tailIndent ++) tailRows

gtkClass :: String -> String
gtkClass "GtkComboBoxText" = "ComboBox"
gtkClass ('G':'t':'k':className) = className
gtkClass className = className
