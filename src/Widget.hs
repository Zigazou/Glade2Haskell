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
module Widget
( Signal (Signal, sName, sHandler)
, Widget (Widget, wClass, wId, wSignals)
) where

{- |
A signal is a couple of a signal name and its associated handler.
-}
data Signal = Signal
    { sName :: String
    , sHandler :: String
    } deriving (Eq, Show)

{- |
A simple Widget class.
-}
data Widget = Widget
    { wClass :: String -- ^ The class name as specified by Glade
    , wId    :: String -- ^ The identifier as specified in a Glade file
    , wSignals :: [Signal] -- ^ The signals and their handlers
    } deriving (Eq, Show)

