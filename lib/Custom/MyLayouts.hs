module Custom.MyLayouts where

-- main library
import XMonad
-- lets you use noBorders and smartBorders
import XMonad.Layout.NoBorders
-- gaps
import XMonad.Layout.Spacing
-- the tabbed layout is nice for certain tasks (with web browsers)
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
-- toggle layouts
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.PerWorkspace
-- for perworkspace to work
import Custom.MyVariables

-- smartBorders removes the border if there is only one screen and only
-- one client on a workspace
-- noBorders removes the border. 
myLayoutHook = spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True 
               $ lessBorders (Combine Difference Screen OnlyScreenFloat)
               $ myLayout

-- I only use tiled and tabbed here because I  toggle
-- fullscreen layout in MyKeys.hs
myLayout = toggleLayouts (noBorders Full) (onWorkspaces [Custom.MyVariables.webWS] (noBorders (tabbed shrinkText Custom.MyVariables.myTabConfig)) tiled ||| ThreeColMid 1 (3/100) (1/2))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
