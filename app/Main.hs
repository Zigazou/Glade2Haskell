module Main where

import System.Environment (getArgs)
import Glade2Haskell (glade2Haskell)

{-|
Basic help for the end user.
-}
simpleHelp :: String
simpleHelp = unlines
    [ "Usage:"
    , "  glade2haskell"
    , "      this simple help"
    , ""
    , "  glade2haskell <typename> <gladefile>"
    , "      generate an Haskell source code file containing a type definition"
    , "      with a load function retrieving each widget contained in the Glade"
    , "      file."
    , ""
    , "Note: the type name and the IDs of each widgets contained in the Glade"
    , "file must comply with Haskell rules because no check is done."
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        (typeName:path:[]) -> putStrLn . unlines =<< glade2Haskell typeName path
        _ -> putStrLn simpleHelp

