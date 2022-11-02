module Custom.MyLayouts where

-- main library
import XMonad
-- lets you use noBorders and smartBorders
import XMonad.Layout.NoBorders
-- the tabbed layout is nice for certain tasks (with web browsers)
import XMonad.Layout.Tabbed
-- perworkspaces lets me set certain layouts on certain workspaces
import XMonad.Layout.PerWorkspace
-- I don't use a bar, so seeing the name of the workspace when I switch to it
-- is nice but is not a must have
import XMonad.Layout.ShowWName

import XMonad.Layout.ToggleLayouts

-- for perworkspace to work
import Custom.MyVariables

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
                  

-- smartBorders removes the border if there is only one screen and only
-- one client on a workspace
-- noBorders removes the border. 
myLayoutHook = showWName' myShowWNameTheme
               $ lessBorders (Combine Difference Screen OnlyScreenFloat)
               $ myLayout

-- I only use tiled and tabbed here because I  toggle
-- fullscreen layout in MyKeys.hs
-- Explanation for this line:
-- the toggleLayouts is to toggle Full with a keybinding
-- the layout "tiled" is available on all workspaces, including workspace 1
-- the layout "(noBorders tiled)" is available on every workspace except for workspace 1
-- where it is replaced by the layout "(noBorders (tabbed shrinkText myTabConfig)"
myLayout = toggleLayouts (noBorders Full)
         $ onWorkspaces ["1:www", "9:null"]
           (tabbed shrinkText myTabConfig) tiled
           ||| (tabbed shrinkText myTabConfig)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
