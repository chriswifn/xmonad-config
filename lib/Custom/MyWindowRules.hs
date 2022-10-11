module Custom.MyWindowRules where

-- main library
import XMonad

-- necessary
import Data.Monoid
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W

-- some floating functions
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)

-- I need my variables to shift clients to specific workspaces
import Custom.MyVariables

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , resource  =? "desktop_window" --> doIgnore
  , isFullscreen                  --> doFullFloat
  , className =? "download"       --> doFloat
  , className =? "error"          --> doFloat
  , className =? "dialog"         --> doFloat
  , className =? "splash"         --> doFloat
  , className =? "MATLAB R2022b - academic use" --> doFloat
  , className =? "firefox"        --> viewShift (myWorkspaces !! 0)
  , className =? "st-256color"    --> viewShift (myWorkspaces !! 2)
  , className =? "Emacs"          --> viewShift (myWorkspaces !! 1)
  , className =? "Alacritty"      --> viewShift (myWorkspaces !! 2)
  , className =? "Zathura"        --> viewShift (myWorkspaces !! 5)
  , className =? "mpv"            --> viewShift (myWorkspaces !! 7)
  , className =? "mus"            --> viewShift (myWorkspaces !! 6)
  , className =? "Virt-manager"   --> viewShift (myWorkspaces !! 8)
  , className =? "file"           --> viewShift (myWorkspaces !! 3)
  , className =? "Pcmanfm"        --> viewShift (myWorkspaces !! 3)
  , className =? "nvim"           --> viewShift (myWorkspaces !! 4)
  , className =? "dev"            --> viewShift (myWorkspaces !! 4)]
  where viewShift = doF . liftM2 (.) W.view W.shift
