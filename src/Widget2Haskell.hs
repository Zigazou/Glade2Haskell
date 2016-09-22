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

import Data.Char (toUpper)

import Widget (Widget (Widget), Signal (Signal))

{- |
`widgets2Haskell` is a simple templating process.
-}
widgets2Haskell :: String -> [Widget] -> ([String], [String], [String])
widgets2Haskell typeName widgets = (typeDef, loader, connect)
    where
        typeDef =
            [ "module " ++ typeName ++ ".Type where"
            , ""
            , "import Graphics.UI.Gtk"
            , ""
            , "data " ++ typeName ++ " = " ++ typeName
            ] ++
            indent "    { " "    , " (generateField <$> widgets) ++
            [ "    }" ]

        loader =
            [ "module " ++ typeName ++ ".Load where"
            , ""
            , "import Graphics.UI.Gtk"
            , "import " ++ typeName ++ ".Type"
            , ""
            , "load :: String -> IO " ++ typeName
            , "load guiPath = do"
            , "    bdr <- builderNew"
            , "    builderAddFromFile bdr guiPath"
            , "    " ++ typeName
            ] ++
            indent "        <$> " "        <*> " (generateCast <$> widgets)

        connect =
            [ "module " ++ typeName ++ ".Connect where"
            , ""
            , "import Graphics.UI.Gtk"
            , "import " ++ typeName ++ ".Type"
            , "import Handlers"
            , ""
            , "connect :: " ++ typeName ++ " -> IO ()"
            , "connect gui = do"
            ] ++
            indent "    " "    " (concat $ generateSignalHandler <$> widgets) ++
            [ "    return ()" ]

{- |
An auxiliary function returning the string for lines defining the fields of
the record.
-}
generateField :: Widget -> String
generateField (Widget cla ide _) = ide ++ " :: " ++ gtkClass cla

{- |
An auxiliary function returning the cast lines of a long applicative call.
-}
generateCast :: Widget -> String
generateCast (Widget cla ide _) =
    "builderGetObject bdr castTo" ++ gtkClass cla ++ " \"" ++ ide ++ "\""

{- |
An auxiliary function returning the assignment lines of signals.
-}
generateSignalHandler :: Widget -> [String]
generateSignalHandler (Widget _ ide signals) = oneSignal <$> signals
    where oneSignal (Signal name handler) = "on ("
                                         ++ ide
                                         ++ " gui) "
                                         ++ gtkSignal name
                                         ++ " ("
                                         ++ handler
                                         ++ " gui)"

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

{- |
Converts a Glade GTK signal name into an Haskell GTK signal name. It generally
consists of removing the '-' chars and adopting camel case style.
-}
gtkSignal :: String -> String
gtkSignal [] = []
gtkSignal ('-':c:cs) = toUpper c:gtkSignal cs
gtkSignal (c:cs) = c:gtkSignal cs

