module Custom.MyLayouts where

-- main library
import XMonad
-- lets you use noBorders and smartBorders
import XMonad.Layout.NoBorders
-- toggle Layouts
import XMonad.Layout.ToggleLayouts (toggleLayouts)

import XMonad.Layout.ShowWName

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font              = "xft:Monoid:size=40"
  , swn_fade              = 1.0
  , swn_bgcolor           = "#323232"
  , swn_color             = "#ffffff"
  }

-- smartBorders removes the border if there is only one screen and only
-- one client on a workspace
-- noBorders removes the border. In this case I only use this for the
-- Fullscreen layout
myLayoutHook = showWName' myShowWNameTheme $ toggleLayouts (noBorders Full) $ smartBorders $ myLayout

-- I only use tiled and noBorders tiled here because I  toggle
-- fullscreen layout in MyKeys.hs
-- All the layouts that I use: tiled (Tall), tiled (Tall) but with no borders, fullscreen (Full)
myLayout = tiled ||| noBorders tiled
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
