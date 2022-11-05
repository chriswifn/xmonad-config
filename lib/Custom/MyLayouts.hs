module Custom.MyLayouts where

-- main library
import XMonad
-- lets you use noBorders and smartBorders
import XMonad.Layout.NoBorders
-- the tabbed layout is nice for certain tasks (with web browsers)
import XMonad.Layout.Tabbed
-- toggle layouts
import XMonad.Layout.ToggleLayouts
-- for perworkspace to work
import Custom.MyVariables

-- smartBorders removes the border if there is only one screen and only
-- one client on a workspace
-- noBorders removes the border. 
myLayoutHook = lessBorders (Combine Difference Screen OnlyScreenFloat)
               $ myLayout

-- I only use tiled and tabbed here because I  toggle
-- fullscreen layout in MyKeys.hs
-- Explanation for this line:
-- the toggleLayouts is to toggle Full with a keybinding
-- the layout "tiled" is available on all workspaces, including workspace 1
-- the layout "(noBorders tiled)" is available on every workspace except for workspace 1
-- where it is replaced by the layout "(noBorders (tabbed shrinkText myTabConfig)"
myLayout = toggleLayouts (noBorders Full) (tiled ||| (tabbed shrinkText Custom.MyVariables.myTabConfig))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
