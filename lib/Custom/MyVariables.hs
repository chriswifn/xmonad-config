module Custom.MyVariables where

-- main library
import XMonad
-- Grid Select to switch betweens applications
import XMonad.Actions.GridSelect
-- tabbed layout
import XMonad.Layout.Tabbed
-- Treeselect
import Data.Tree
import qualified XMonad.Actions.TreeSelect as TS

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
-- myWorkspaces    = ["1:www","2:emacs","3:term","4:file","5:dev","6:doc","7:mus","8:vid","9:null"]
myWorkspaces = [ Node "browser"
                 [ Node "firefox" []]
               , Node "programming"
                 [ Node "emacs" []
                 , Node "terminal" []
                 , Node "dev" []
                 , Node "matlab" []
                 , Node "doc" []]
               , Node "home"
                 [ Node "file" []
                 , Node "video" []
                 , Node "music" []
                 , Node "virtual" []]
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
                  

