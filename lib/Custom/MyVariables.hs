module Custom.MyVariables where

-- main library
import XMonad
-- tabbed layout
import XMonad.Layout.Tabbed
-- Treeselect
import Data.Tree
import qualified XMonad.Actions.TreeSelect as TS
-- Grid Select to switch betweens applications
import XMonad.Actions.GridSelect

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
myWorkspaces :: Forest String
myWorkspaces = [ Node "home"
                 [ Node "browser"
                   [ Node "firefox" []
                   ]
                 , Node "programming"
                   [ Node "emacs" []
                   , Node "doc" []
                   ]
                 , Node "terminal" 
                   [ Node "dev" []
                   , Node "file-terminal" []
                   ]
                 , Node "file-management" []
                 , Node "entertainment"
                   [ Node "music" []
                   , Node "video" []
                   , Node "image" []
                   ]
                 , Node "virtual" []
                 , Node "office" []
                 ]
               , Node "work-programming"
                 [ Node "browser" []
                 , Node "programming"
                   [ Node "emacs" []
                   , Node "doc" []
                   , Node "matlab" []
                   ]
                 , Node "reference" []
                 , Node "terminal"
                   [ Node "tmux-session" []
                   , Node "trash-term" []
                   ]
                 ]
               , Node "work-sysad"
                 [ Node "browser" []
                 , Node "emacs" []
                 , Node "terminal" []
                 ]
               ]

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
myFont = "xft:Monoid:regular:size=10:antialias=true:hinting=true"

myTabConfig = def { fontName = myFont 
                  , activeColor = "#323232"
                  , inactiveColor = "#1e1e1e"
                  , activeBorderColor = "#323232"
                  , inactiveBorderColor = "#1e1e1e"
                  , activeTextColor = "#ccdfe7"
                  , inactiveTextColor = "#ccdfe7"
                  }

myTSConfig = TS.TSConfig { TS.ts_hidechildren = False
                         , TS.ts_background   = 0x000000
                         , TS.ts_font         = myFont 
                         , TS.ts_node         = (0xccdfe7, 0x323232)
                         , TS.ts_nodealt      = (0xccdfe7, 0x1e1e1e)
                         , TS.ts_highlight    = (0xffffff, 0xf78fe7)
                         , TS.ts_extra        = 0xffffff
                         , TS.ts_node_width   = 200 
                         , TS.ts_node_height  = 30
                         , TS.ts_originX      = 0
                         , TS.ts_originY      = 0
                         , TS.ts_indent       = 80
                         , TS.ts_navigate     = TS.defaultNavigation
                         }
                  
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                (0x1e,0x1e,0x1e) -- lowest inactive bg
                (0x32,0x32,0x32) -- highest inactive bg
                (0xf7,0x8f,0xe7) -- active bg
                (0xff,0xff,0xff) -- inactive fg
                (0x00,0x00,0x00) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 30
    , gs_cellwidth    = 1000
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont 
    }

