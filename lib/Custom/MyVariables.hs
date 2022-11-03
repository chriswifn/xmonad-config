module Custom.MyVariables where

-- main library
import XMonad
-- Grid Select to switch betweens applications
import XMonad.Actions.GridSelect
-- Show workspace name
import XMonad.Layout.ShowWName
-- tabbed layout
import XMonad.Layout.Tabbed

-- st is objectively the best terminal
myTerminal :: String
myTerminal = "st "

-- emacs is objectively the best text editor
myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "

-- Alt is a stupid modifier key, so use Super
myModMask :: KeyMask
myModMask = mod4Mask

-- sometimes useful
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- sometimes useful
myClickJustFocuses :: Bool
myClickJustFocuses = False 

-- workspace names
myWorkspaces    = ["1:www","2:emacs","3:term","4:file","5:dev","6:doc","7:mus","8:vid","9:null"]

-- border widht: a nice big border
myBorderWidth :: Dimension 
myBorderWidth = 1

-- black is my primary background color, inactive border should blend into the background
myNormalBorderColor :: String
myNormalBorderColor = "#323232"

-- a nice color that fints the theme
myFocusedBorderColor :: String
myFocusedBorderColor = "#f78fe7"

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                (0x1e,0x1e,0x1e) -- lowest inactive bg
                (0x32,0x32,0x32) -- highest inactive bg
                (0xf7,0x8f,0xe7) -- active bg
                (0xff,0xff,0xff) -- inactive fg
                (0x32,0x32,0x32) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = "xft:Monoid:regular:size=10:antialias=true:hinting=true"
    }

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font              = "xft:Monoid:size=11"
  , swn_fade              = 1.0
  , swn_bgcolor           = "#323232"
  , swn_color             = "#ffffff"
  }

myTabConfig = def { fontName = "xft:Monoid:regular:size=10:antialias=true:hinting=true"
                  , activeColor = "#323232"
                  , inactiveColor = "#1e1e1e"
                  , activeBorderColor = "#323232"
                  , inactiveBorderColor = "#1e1e1e"
                  , activeTextColor = "#ccdfe7"
                  , inactiveTextColor = "#ccdfe7"
                  }
                  

