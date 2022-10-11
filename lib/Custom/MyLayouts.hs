module Custom.MyLayouts where

-- main library
import XMonad
-- lets you use noBorders and smartBorders
import XMonad.Layout.NoBorders
-- toggle Layouts
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.Layout.Tabbed

import XMonad.Layout.ShowWName

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font              = "xft:Monoid:size=20"
  , swn_fade              = 1.0
  , swn_bgcolor           = "#323232"
  , swn_color             = "#ffffff"
  }

myTabConfig = def { fontName = "xft:Monoid:regular:size=10:antialias=true:hinting=true"
                  , activeColor = "#323232"
                  , inactiveColor = "#1e1e1e"
                  , activeBorderColor = "#323232"
                  , inactiveBorderColor = "#1e1e1e"
                  , activeTextColor = "#ffffff"
                  , inactiveTextColor = "#ccdfe7"
                  }
                  

-- smartBorders removes the border if there is only one screen and only
-- one client on a workspace
-- noBorders removes the border. In this case I only use this for the
-- Fullscreen layout
myLayoutHook = showWName' myShowWNameTheme $ toggleLayouts (noBorders Full) $ smartBorders $ myLayout

-- I only use tiled and noBorders tiled here because I  toggle
-- fullscreen layout in MyKeys.hs
-- All the layouts that I use: tiled (Tall), tiled (Tall) but with no borders, fullscreen (Full)
myLayout = noBorders tiled ||| noBorders (tabbed shrinkText myTabConfig)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
