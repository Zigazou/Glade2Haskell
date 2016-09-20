module Widget (Widget (Widget, wClass, wId)) where

data Widget = Widget { wClass :: String
                     , wId :: String
                     } deriving (Eq, Show)

