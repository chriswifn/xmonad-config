module Custom.MyWindowRules where

-- main library
import XMonad

-- necessary
import Data.Monoid
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W

-- some floating functions
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.InsertPosition

-- I need my variables to shift clients to specific workspaces
import Custom.MyVariables

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
  , className =? "MATLAB R2022b - academic use" --> doCenterFloat]
