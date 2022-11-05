module Custom.MyWindowRules where

-- main library
import XMonad

-- necessary
import Data.Monoid
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import XMonad.Actions.TreeSelect

-- some floating functions
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.InsertPosition

-- I need my variables to shift clients to specific workspaces
import Custom.MyVariables

myWorkspacesForWindowRules = toWorkspaces myWorkspaces

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = insertPosition Master Newer <> composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , resource  =? "desktop_window" --> doIgnore
  , isFullscreen                  --> doFullFloat
  , className =? "download"       --> doFloat
  , className =? "error"          --> doFloat
  , className =? "dialog"         --> doFloat
  , className =? "splash"         --> doFloat
  , className =? "MATLAB R2022b - academic use" --> viewShift (toWorkspaces myWorkspaces !! 8)
  , className =? "mpv"            --> viewShift (myWorkspacesForWindowRules !! 11)
  , className =? "mus"            --> viewShift (myWorkspacesForWindowRules !! 12)
  , className =? "Picard"         --> viewShift (myWorkspacesForWindowRules !! 12)
  , className =? "Virt-manager"   --> viewShift (myWorkspacesForWindowRules !! 13)
  , className =? "file"           --> viewShift (myWorkspacesForWindowRules !! 5)
  , className =? "Pcmanfm"        --> viewShift (myWorkspacesForWindowRules !! 10)]
  where viewShift = doF . liftM2 (.) W.view W.shift
