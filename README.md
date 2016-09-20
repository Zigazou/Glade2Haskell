Glade2Haskell
=============

Glade2Haskell reads a Glade file and extracts the widget definitions in order
to create an Haskell source code file which can load a Glade file and retrieve
each widget to fill a record.

It is especially useful when dealing with lots of widgets that you have to
access for updating and/or reading. It saves you the pain of writing the
definition and the cast of each widget.

Example
-------

Running `glade2haskell NecControlGUI example/neccontrolgui.glade` will generate
the following source code:

    module NecControlGUI where

    import Graphics.UI.Gtk

    data NecControlGUI = NecControlGUI
        { adjBalance :: Adjustment
        , adjBass :: Adjustment
        , adjBlackLevel :: Adjustment
        , adjBrightness :: Adjustment
        , adjColorTemperature :: Adjustment
        , adjContrast :: Adjustment
        , adjMenuDisplayTime :: Adjustment
        , adjSharpness :: Adjustment
        , adjTreble :: Adjustment
        , adjVolume :: Adjustment
        , winNecControl :: Window
        , nbkSections :: Notebook
        , grdScreen :: Grid
        , lblIPAddress :: Label
        , entIPAddress :: Entry
        , btnConnection :: Button
        , lblScreen :: Label
        , grdVideo :: Grid
        , lblVideoInput :: Label
        , cbtVideoInput :: ComboBox
        , lblPIPInput :: Label
        , cbtPIPInput :: ComboBox
        , lblVideo :: Label
        , grdImage :: Grid
        , lblBrightness :: Label
        , lblContrast :: Label
        , lblSharpness :: Label
        , sclSharpness :: Scale
        , sclContrast :: Scale
        , sclBrightness :: Scale
        , lblBlackLevel :: Label
        , sclBlackLevel :: Scale
        , lblColorTemperature :: Label
        , sclColorTemperature :: Scale
        , lblGamma :: Label
        , cbtGamma :: ComboBox
        , lblImage :: Label
        , grdAudio :: Grid
        , lblBalance :: Label
        , lblTreble :: Label
        , lblBass :: Label
        , sclBalance :: Scale
        , sclTreble :: Scale
        , sclBass :: Scale
        , lblVolume :: Label
        , sclVolume :: Scale
        , lblSound :: Label
        , grdOsd :: Grid
        , lblLanguage :: Label
        , cbtLanguage :: ComboBox
        , lblMenuDisplayTime :: Label
        , sclMenuDisplayTime :: Scale
        , lblOsd :: Label
        }

    loadNecControlGUI :: String -> IO NecControlGUI
    loadNecControlGUI guiPath = do
        bdr <- builderNew
        builderAddFromFile bdr guiPath
        NecControlGUI
                <$> builderGetObject bdr castToAdjustment "adjBalance"
                <*> builderGetObject bdr castToAdjustment "adjBass"
                <*> builderGetObject bdr castToAdjustment "adjBlackLevel"
                <*> builderGetObject bdr castToAdjustment "adjBrightness"
                <*> builderGetObject bdr castToAdjustment "adjColorTemperature"
                <*> builderGetObject bdr castToAdjustment "adjContrast"
                <*> builderGetObject bdr castToAdjustment "adjMenuDisplayTime"
                <*> builderGetObject bdr castToAdjustment "adjSharpness"
                <*> builderGetObject bdr castToAdjustment "adjTreble"
                <*> builderGetObject bdr castToAdjustment "adjVolume"
                <*> builderGetObject bdr castToWindow "winNecControl"
                <*> builderGetObject bdr castToNotebook "nbkSections"
                <*> builderGetObject bdr castToGrid "grdScreen"
                <*> builderGetObject bdr castToLabel "lblIPAddress"
                <*> builderGetObject bdr castToEntry "entIPAddress"
                <*> builderGetObject bdr castToButton "btnConnection"
                <*> builderGetObject bdr castToLabel "lblScreen"
                <*> builderGetObject bdr castToGrid "grdVideo"
                <*> builderGetObject bdr castToLabel "lblVideoInput"
                <*> builderGetObject bdr castToComboBox "cbtVideoInput"
                <*> builderGetObject bdr castToLabel "lblPIPInput"
                <*> builderGetObject bdr castToComboBox "cbtPIPInput"
                <*> builderGetObject bdr castToLabel "lblVideo"
                <*> builderGetObject bdr castToGrid "grdImage"
                <*> builderGetObject bdr castToLabel "lblBrightness"
                <*> builderGetObject bdr castToLabel "lblContrast"
                <*> builderGetObject bdr castToLabel "lblSharpness"
                <*> builderGetObject bdr castToScale "sclSharpness"
                <*> builderGetObject bdr castToScale "sclContrast"
                <*> builderGetObject bdr castToScale "sclBrightness"
                <*> builderGetObject bdr castToLabel "lblBlackLevel"
                <*> builderGetObject bdr castToScale "sclBlackLevel"
                <*> builderGetObject bdr castToLabel "lblColorTemperature"
                <*> builderGetObject bdr castToScale "sclColorTemperature"
                <*> builderGetObject bdr castToLabel "lblGamma"
                <*> builderGetObject bdr castToComboBox "cbtGamma"
                <*> builderGetObject bdr castToLabel "lblImage"
                <*> builderGetObject bdr castToGrid "grdAudio"
                <*> builderGetObject bdr castToLabel "lblBalance"
                <*> builderGetObject bdr castToLabel "lblTreble"
                <*> builderGetObject bdr castToLabel "lblBass"
                <*> builderGetObject bdr castToScale "sclBalance"
                <*> builderGetObject bdr castToScale "sclTreble"
                <*> builderGetObject bdr castToScale "sclBass"
                <*> builderGetObject bdr castToLabel "lblVolume"
                <*> builderGetObject bdr castToScale "sclVolume"
                <*> builderGetObject bdr castToLabel "lblSound"
                <*> builderGetObject bdr castToGrid "grdOsd"
                <*> builderGetObject bdr castToLabel "lblLanguage"
                <*> builderGetObject bdr castToComboBox "cbtLanguage"
                <*> builderGetObject bdr castToLabel "lblMenuDisplayTime"
                <*> builderGetObject bdr castToScale "sclMenuDisplayTime"
                <*> builderGetObject bdr castToLabel "lblOsd"

Usage
-----

Showing help:

    glade2haskell

Generate an Haskell source code file on standard output:

    glade2haskell <typename> <gladefile>

Notes
-----

The type name and the IDs of each widgets contained in the Glade file must
comply with Haskell names writing rules because no check is done.

