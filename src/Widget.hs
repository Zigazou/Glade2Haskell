{- |
Module      : Widget
Description : A type representing a Widget
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This is a very simple type holding the Widget class and its identifier.
-}
module Widget (Widget (Widget, wClass, wId)) where

{- |
A simple Widget class.
-}
data Widget = Widget
    { wClass :: String -- ^ The class name as specified by Glade
    , wId    :: String -- ^ The identifier as specified in a Glade file
    } deriving (Eq, Show)

