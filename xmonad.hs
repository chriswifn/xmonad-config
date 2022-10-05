-- Base 
import XMonad
import qualified XMonad.StackSet as W
import System.Exit

-- Actions
import XMonad.Actions.CycleWS (nextScreen, prevScreen)

-- Data
import Data.Monoid
import qualified Data.Map as M

-- Control
import Control.Monad (liftM2)

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts (toggleLayouts)

-- Utilities
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)

myTerminal :: String
myTerminal = "st "

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "

myModMask :: KeyMask
myModMask = mod4Mask

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False 

myClickJustFocuses :: Bool
myClickJustFocuses = True 

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myBorderWidth :: Dimension 
myBorderWidth = 5

myNormalBorderColor :: String
myNormalBorderColor = "#000000"

myFocusedBorderColor :: String
myFocusedBorderColor = "#f78fe7"

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xsetroot -cursor_name left_pr"
  spawnOnce "setxkbmap -option caps:escape"
  spawnOnce "/usr/bin/lxpolkit"
  spawnOnce "/usr/bin/emacs --daemon"
  setWMName "LG3D"

-- smartBorders removes the border if there is only one screen and only
-- one client on a workspace
-- noBorders removes the border. In this case I only use this for the
-- Fullscreen layout
myLayoutHook = toggleLayouts (noBorders Full) $ smartBorders $ myLayout

myLayout = tiled ||| noBorders Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

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
  , className =? "firefox"        --> viewShift (myWorkspaces !! 1)
  , className =? "st-256color"    --> viewShift (myWorkspaces !! 3)
  , className =? "Emacs"          --> viewShift (myWorkspaces !! 2)
  , className =? "Alacritty"      --> viewShift (myWorkspaces !! 3)
  , className =? "Zathura"        --> viewShift (myWorkspaces !! 6)
  , className =? "mpv"            --> viewShift (myWorkspaces !! 8)
  , className =? "mus"            --> viewShift (myWorkspaces !! 7)
  , className =? "Virt-manager"   --> viewShift (myWorkspaces !! 9)
  , className =? "file"           --> viewShift (myWorkspaces !! 4)
  , className =? "nvim"           --> viewShift (myWorkspaces !! 5)
  , className =? "dev"            --> viewShift (myWorkspaces !! 5)]
  where viewShift = doF . liftM2 (.) W.greedyView W.shift

myKeys = \c -> mkKeymap c $
  -- essential keybindings
  [ ("M-S-<Return>", spawn $ terminal c)
  , ("M-S-p", spawn $ "dmenu_run -l 20")
  , ("M-S-c", kill)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-S-<Space>", setLayout $ XMonad.layoutHook c)
  , ("M-n", refresh)
  , ("M-<Tab>", windows W.focusDown)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-m", windows W.focusMaster)
  , ("M-S-m", windows W.swapMaster)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-S-t", withFocused $ windows . W.sink)
  , ("M-.", nextScreen)
  , ("M-,", prevScreen)
  , ("M-d", sendMessage (IncMasterN 1))
  , ("M-u", sendMessage (IncMasterN (-1)))
  , ("M-q", spawn $ "xmonad --recompile; xmonad --restart")
  , ("M-S-q", io (exitWith ExitSuccess))]
  ++

  -- Switch to workspace
  [ ("M-1", windows $ W.greedyView $ myWorkspaces !! 1)
  , ("M-2", windows $ W.greedyView $ myWorkspaces !! 2)
  , ("M-3", windows $ W.greedyView $ myWorkspaces !! 3)
  , ("M-4", windows $ W.greedyView $ myWorkspaces !! 4)
  , ("M-5", windows $ W.greedyView $ myWorkspaces !! 5)
  , ("M-6", windows $ W.greedyView $ myWorkspaces !! 6)
  , ("M-7", windows $ W.greedyView $ myWorkspaces !! 7)
  , ("M-8", windows $ W.greedyView $ myWorkspaces !! 8)
  , ("M-9", windows $ W.greedyView $ myWorkspaces !! 9)]
  ++

  -- Send client to workspace
  [ ("M-S-1", windows $ W.shift $ myWorkspaces !! 1)
  , ("M-S-2", windows $ W.shift $ myWorkspaces !! 2)
  , ("M-S-3", windows $ W.shift $ myWorkspaces !! 3)
  , ("M-S-4", windows $ W.shift $ myWorkspaces !! 4)
  , ("M-S-5", windows $ W.shift $ myWorkspaces !! 5)
  , ("M-S-6", windows $ W.shift $ myWorkspaces !! 6)
  , ("M-S-7", windows $ W.shift $ myWorkspaces !! 7)
  , ("M-S-8", windows $ W.shift $ myWorkspaces !! 8)
  , ("M-S-9", windows $ W.shift $ myWorkspaces !! 9)
  ]
  ++

  -- Emacs programs
  [ ("M-e e", spawn $ myEmacs)
  , ("M-e b", spawn $ myEmacs ++ ("--eval '(ibuffer)'"))
  , ("M-e d", spawn $ myEmacs ++ ("--eval '(dired nil)'"))
  , ("M-e t", spawn $ myEmacs ++ ("--eval '(+vterm/here nil)'"))]
  ++

  -- Terminal programs
  [ ("M-t t", spawn $ myTerminal ++ ("-c 'dev' -e tmux"))
  , ("M-t n", spawn $ myTerminal ++ ("-c 'nvim' -e nvim"))
  , ("M-t h", spawn $ myTerminal ++ ("-e htop"))
  , ("M-t a", spawn $ myTerminal ++ ("-c 'mus' -e cmus"))
  , ("M-t r", spawn $ myTerminal ++ ("-c 'file' -e lf-run"))
  , ("M-t p", spawn $ myTerminal ++ ("-e pulsemixer"))
  ]
  ++

  -- dmenu scripts
  [ ("M-p a", spawn $ "dmenu_run -l 20")
  , ("M-p m", spawn $ "monitors")
  , ("M-p b", spawn $ "bookmarks")
  , ("M-p k", spawn $ "keyboard")
  , ("M-p s", spawn $ "maimmenu")
  , ("M-p i", spawn $ "network")
  , ("M-p l", spawn $ "logoutmenu")
  , ("M-p p", spawn $ "passmenu -l 20 -p 'Choose password: '")
  , ("M-p w", spawn $ "connectwifi")
  , ("M-p e", spawn $ "emojipicker")
  , ("M-p v", spawn $ "audiodevice")
  , ("M-p c", spawn $ "audioinputdevice")
  ]
  ++

  -- Gui programs
  [ ("M-g", spawn $ "firefox")
  , ("M-z", spawn $ "zathura")
  , ("M-S-f", spawn $ "pcmanfm")
  , ("M-v", spawn $ "virt-manager")
  , ("M-S-s", spawn $ "slock")]
  ++

  -- scripts
  [ ("M-<F1>", spawn $ "volume mute")
  , ("M-<F2>", spawn $ "volume down")
  , ("M-<F3>", spawn $ "volume up")
  , ("M-<F4>", spawn $ "microphone mute")
  , ("M-<F5>", spawn $ "microphone down")
  , ("M-<F6>", spawn $ "microphone up")
  , ("M-<F7>", spawn $ "brightness down")
  , ("M-<F8>", spawn $ "brightness up")
  , ("M-<F9>", spawn $ "gamma")]

main :: IO ()
main = xmonad defaults

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
  manageHook = myManageHook <+> manageDocks
  }
