-- Base (these imports are necessary)
import XMonad

-- some imports for the ManageHook
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.InsertPosition

-- Custom (my custom libraries)
import Custom.MyVariables
import Custom.MyAutostart
import Custom.MyLayouts
import Custom.MyWindowRules
import Custom.MyKeys

-- the main function: this is where the magic happens
main :: IO ()
main = xmonad $ defaults

defaults = def {
  -- simple stuff
  terminal = myTerminal,
  focusFollowsMouse = myFocusFollowsMouse,
  clickJustFocuses = myClickJustFocuses,
  borderWidth = myBorderWidth,
  modMask = myModMask,
  workspaces = myWorkspaces,
  normalBorderColor  = myNormalBorderColor,
  focusedBorderColor = myFocusedBorderColor,

  -- keybindings
  keys = myKeys,

  -- hooks
  layoutHook = myLayoutHook,
  startupHook = myStartupHook,
  manageHook = insertPosition Master Newer <> myManageHook <+> manageDocks
  }
