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

generalManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
generalManageHook = insertPosition End Newer <> composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , resource  =? "desktop_window" --> doIgnore
  , isFullscreen                  --> doFullFloat
  , className =? "download"       --> doFloat
  , className =? "error"          --> doFloat
  , className =? "dialog"         --> doFloat
  , className =? "MATLAB R2022b - academic use" --> doFloat
  , className =? "splash"         --> doFloat]

manageZoomHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
manageZoomHook =
  composeAll $
    [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat,
      (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
    ]
  where
    zoomClassName = "zoom"
    tileTitles =
      [ "Zoom - Free Account", -- main window
        "Zoom - Licensed Account", -- main window
        "Zoom", -- meeting window on creation
        "Zoom Meeting" -- meeting window shortly after creation
      ]
    shouldFloat title = title `notElem` tileTitles
    shouldSink title = title `elem` tileTitles
    doSink = (ask >>= doF . W.sink) <+> doF W.swapDown

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = generalManageHook <+> manageZoomHook
