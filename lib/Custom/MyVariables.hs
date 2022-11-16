module Custom.MyVariables where

-- main library
import XMonad
-- tabbed layout
import XMonad.Layout.Tabbed
-- Treeselect
import Data.Tree
import qualified XMonad.Actions.TreeSelect as TS
-- ShowWName
import XMonad.Layout.ShowWName
-- prompts
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch

-- st is objectively the best terminal
myTerminal :: String
myTerminal = "st "

-- emacs is objectively the best text editor
myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "

myBrowser :: String
myBrowser = "firefox"

-- Alt is a stupid modifier key, so use Super
myModMask :: KeyMask
myModMask = mod4Mask

-- sometimes useful
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- sometimes useful
myClickJustFocuses :: Bool
myClickJustFocuses = False 

myXPConfig = def
  { position          = Bottom
  , searchPredicate   = fuzzyMatch
  , sorter            = fuzzySort
  , alwaysHighlight   = True
  , borderColor       = "#1e1e1e" 
  , bgColor           = "#1e1e1e" 
  , fgColor           = "#ffffff" 
  , bgHLight          = "#f78fe7" 
  , fgHLight          = "#000000" 
  , defaultText       = ""
  , font              = myFont 
  , height            = 20
  , promptBorderWidth = 5
  -- , maxComplColumns   = Just 1
  , promptKeymap      = emacsLikeXPKeymap
  }

-- workspace names
standard = [Node (show n ) [] | n <- [1..3]]
myWorkspaces = [ Node "main"
                 [ Node "research" standard 
                 , Node "development" standard
                 , Node "reference" standard
                 , Node "null" standard
                 ]
               , Node "work-programming"
                 [ Node "research" standard
                 , Node "development" standard
                 , Node "reference" standard
                 , Node "null" standard
                 ]
               , Node "work-sysad"
                 [ Node "research" standard
                 , Node "development" standard
                 , Node "reference" standard
                 , Node "null" standard
                 ]
               ]

-- for movement using number keys
-- myMainworkspaces = drop 1 (take 10 (TS.toWorkspaces myWorkspaces))
-- myWorkworkspaces = drop 11 (take 20 (TS.toWorkspaces myWorkspaces))

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
  [ Node (TS.TSNode "volume" "change the volume" (spawn "st -c 'pulse' -e pulsemixer"))
    [ Node (TS.TSNode "microphone" "toggle mute (microphone)" (spawn "pactl set-source-mute 0 'toggle'")) []
    , Node (TS.TSNode "speaker/headphone" "toggle mute (speaker/headphone)" (spawn "pactl set-sink-mute 0 'toggle'")) []
    ]
  , Node (TS.TSNode "brightness" "change the brightness" (return ())) 
    [ Node (TS.TSNode "bright" "set brightness to 100%" (spawn "brightnessctl set 100%")) []
    , Node (TS.TSNode "medium" "set brightness to 50%" (spawn "brightnessctl set 50%")) []
    , Node (TS.TSNode "low" "set brightness to 10%" (spawn "brightnessctl set 10%")) []
    ]
  , Node (TS.TSNode "blue-light-filter" "toggle bluelight filter" (spawn "gamma")) []
  , Node (TS.TSNode "logout" "menu for logout options" (return ())) 
    [ Node (TS.TSNode "shutdown" "shutdown the system" (spawn "systemctl poweroff")) []
    , Node (TS.TSNode "reboot" "reboot the system" (spawn "systemctl reboot")) []
    , Node (TS.TSNode "lock" "lock the system with slock" (spawn "slock")) []
    ]
  ]

-- border widht: a nice big border
myBorderWidth :: Dimension 
myBorderWidth = 1

-- black is my primary background color, inactive border should blend into the background
myNormalBorderColor :: String
myNormalBorderColor = "#323232"

-- a nice color that fints the theme
myFocusedBorderColor :: String
myFocusedBorderColor = "#f78fe7"

myFont :: String
-- myFont = "xft:Monoid:regular:size=9:antialias=true:hinting=true"
myFont = "xft:Terminus (TTF):regular:size=12:antialias=true:hinting=true"

myTabConfig = def { fontName = myFont 
                  , activeColor = "#323232"
                  , inactiveColor = "#1e1e1e"
                  , activeBorderColor = "#323232"
                  , inactiveBorderColor = "#1e1e1e"
                  , activeTextColor = "#ccdfe7"
                  , inactiveTextColor = "#ccdfe7"
                  }

myTSConfig = TS.TSConfig { TS.ts_hidechildren = False
                         , TS.ts_background   = 0xdd000000
                         , TS.ts_font         = myFont 
                         , TS.ts_node         = (0xffccdfe7, 0xff323232)
                         , TS.ts_nodealt      = (0xffccdfe7, 0xff1e1e1e)
                         , TS.ts_highlight    = (0xffffffff, 0xfff78fe7)
                         , TS.ts_extra        = 0xffffffff
                         , TS.ts_node_width   = 200 
                         , TS.ts_node_height  = 30
                         , TS.ts_originX      = 0
                         , TS.ts_originY      = 0
                         , TS.ts_indent       = 80
                         , TS.ts_navigate     = TS.defaultNavigation
                         }
                  
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font              = myFont 
  , swn_fade              = 1.0
  , swn_bgcolor           = "#323232"
  , swn_color             = "#ffffff"
  }
