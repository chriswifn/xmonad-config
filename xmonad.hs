-- Base (these imports are necessary)
import XMonad

-- Custom (my custom libraries)
import Custom.MyVariables
import Custom.MyAutostart
import Custom.MyLayouts
import Custom.MyWindowRules
import Custom.MyKeys
import Custom.MySwallow

import XMonad.Actions.TreeSelect
import XMonad.Hooks.WorkspaceHistory

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
  workspaces = toWorkspaces myWorkspaces,
  normalBorderColor  = myNormalBorderColor,
  focusedBorderColor = myFocusedBorderColor,

  -- keybindings
  keys = myKeys,

  -- hooks
  layoutHook = myLayoutHook,
  handleEventHook = myHandleEventHook, 
  startupHook = myStartupHook,
  manageHook = myManageHook,
  logHook = workspaceHistoryHook
  }
