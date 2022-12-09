module Custom.MyKeys where

-- main libraries
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Run

-- some other necessary imports
-- exit
import System.Exit
-- cycle and move betweens monitors
import XMonad.Actions.CycleWS (toggleWS)
-- copy
import XMonad.Actions.CopyWindow (copy, copyToAll, killAllOtherCopies, kill1)
-- eazy keybindings
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)
-- toggle fullscreen
import XMonad.Layout.ToggleLayouts (ToggleLayout (Toggle))
-- for some themes
import Custom.MyVariables
-- run or raise
import XMonad.Actions.WindowGo
-- dynamic projects
import XMonad.Actions.DynamicProjects

import XMonad.Actions.EasyMotion (selectWindow)

-- prompts for open windows and workspaces
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.RunOrRaise

-- scratchpads
import XMonad.Util.NamedScratchpad

-- Keymaps
myKeys = \c -> mkKeymap c $
  -- ESSENTIAL KEYBINDINGS 

  -- spawn terminal
  [ ("M-x p", spawn $ "dmenu_run -l 10 -p 'Application: '")

  -- kill a window
  , ("M-x c", kill1)

  -- switch between layouts
  , ("M-x g n", sendMessage NextLayout)

  -- set a layout
  , ("M-x g l", setLayout $ XMonad.layoutHook c)

  -- refresh
  , ("M-x n", refresh)

  -- make a client sticky
  , ("M-x s", windows copyToAll)

  -- make a client unsticky
  , ("M-x x s", killAllOtherCopies)

  -- focus between windows another way
  , ("M-x j", windows W.focusDown)
  , ("M-x k", windows W.focusUp)

  -- switch focus to the master client
  , ("M-x m", windows W.focusMaster)

  -- switch a client in the stack with the master client
  , ("M-x x m", windows W.swapMaster)

  -- swap windows up and down
  , ("M-x x j", windows W.swapDown)
  , ("M-x x k", windows W.swapUp)

  -- make client smaller
  , ("M-x h", sendMessage Shrink)

  -- make client bigger
  , ("M-x l", sendMessage Expand)

  -- force floating client back to tiling
  , ("M-x t", withFocused $ windows . W.sink)

  -- make a client fullscreen
  , ("M-x f", sendMessage $ Toggle "Full")

  -- increment the amount of master nodes
  , ("M-x i", sendMessage (IncMasterN 1))

  -- decrement the amount of master nodes
  , ("M-x u", sendMessage (IncMasterN (-1)))

  -- recompile and restart XMonad
  , ("M-x r", spawn $ "xmonad --recompile; xmonad --restart")

  -- quit XMonad
  , ("M-x q", io (exitWith ExitSuccess))]
  ++

  -- STUFF FOR DYNAMIC PROJECTS (mainly prompts)
  [ ("M-p w", windowPrompt myXPConfig Goto allWindows)

  , ("M-p b", windowPrompt myXPConfig BringCopy allWindows)

  , ("M-p g", workspacePrompt myXPConfig (windows . W.view))

  , ("M-p c", workspacePrompt myXPConfig (windows . copy))

  , ("M-p f", selectWindow myemConf >>=  (`whenJust` windows . W.focusWindow))

  , ("M-p k", selectWindow myemkillConf >>=  (`whenJust` killWindow))

  , ("M-p p", switchProjectPrompt myXPConfig)

  , ("M-p r", renameProjectPrompt myXPConfig)

  , ("M-p d", changeProjectDirPrompt myXPConfig)

  , ("M-p s", shiftToProjectPrompt myXPConfig)

  , ("M-p t", toggleWS)

  , ("M-p l", runOrRaisePrompt myXPConfig)

  ]
  ++

  -- WORKSPACE STUFF
  -- [ ("M-" ++ m ++ k, windows $ f i)
  --       | (i, k) <- zip (myMainworkspaces) (map show [1 :: Int ..])
  --       , (f, m) <- [(W.greedyView, ""), (W.shift, "S-"), (copy, "S-C-")]]
  -- ++

  -- [ ("M-M1-" ++ m ++ k, windows $ f i)
  --       | (i, k) <- zip (myWorkworkspaces) (map show [1 :: Int ..])
  --       , (f, m) <- [(W.greedyView, ""), (W.shift, "S-"), (copy, "S-C-")]]
  -- ++

  -- SCRATCHPADS
  [ ("M-s h", namedScratchpadAction scratchpads "htop")
  , ("M-s p", namedScratchpadAction scratchpads "pulsemixer")
  ]
  ++

  -- EMACS PROGRAMS
  [ ("M-e e", raiseNextMaybe (spawn myEmacs) (className =? "Emacs"))
  , ("M-e b", spawn $ myEmacs ++ ("--eval '(ibuffer)'"))
  , ("M-e d", spawn $ myEmacs ++ ("--eval '(dired nil)'"))
  , ("M-e t", spawn $ myEmacs ++ ("--eval '(+vterm/here nil)'"))
  , ("M-e m", spawn $ myEmacs ++ ("--eval '(emms-smart-browse)'"))]
  ++

  -- TERMINAL PROGRAMS
  [ ("M-t t", raiseNextMaybe (spawn "st") (className =? "st-256color" <||> title =? "tmux" <||> title =? "devtmux"))
  , ("M-t n", spawn $ terminal c)
  , ("M-t d", raiseMaybe (runInTerm "-T nvim" "nvim") (title =? "nvim"))
  , ("M-t a", raiseMaybe (runInTerm "-T cmus" "cmus") (title =? "cmus"))
  , ("M-t r", raiseMaybe (runInTerm "-T lf" "lf-run") (title =? "lf"))
  ]
  ++

  -- OPEN WITH DMENU
  [ ("M-o p", spawn $ "open_with_dmenu pdf")
  , ("M-o i", spawn $ "open_with_dmenu image")
  , ("M-o o", spawn $ "open_with_dmenu doc")
  , ("M-o v", spawn $ "open_with_dmenu video")
  ]
  ++

  -- DMENU SCRIPTS (keyboard and touchpad are not dmenu scripts)
  [ ("M-d a", spawn $ "dmenu_run -l 10 -p 'Application: '")
  , ("M-d m", spawn $ "monitors")
  , ("M-d b", spawn $ "bookmarks")
  , ("M-d k", spawn $ "keyboard")
  , ("M-d s", spawn $ "maimmenu")
  , ("M-d i", spawn $ "network")
  , ("M-d l", spawn $ "logoutmenu")
  , ("M-d p", spawn $ "passmenu -l 20 -p 'Password: '")
  , ("M-d w", spawn $ "connectwifi")
  , ("M-d e", spawn $ "emojipicker")
  , ("M-d v", spawn $ "audiodevice")
  , ("M-d c", spawn $ "audioinputdevice")
  , ("M-d t", spawn $ "touchpad")
  ]
  ++

  -- GUI PROGRAMS
  [ ("M-g g", raiseNextMaybe (spawn "firefox") (className =? "firefox"))
  , ("M-g b", spawn "firefox")
  , ("M-g z", runOrRaise "zathura" (className =? "Zathura"))
  , ("M-g f", runOrRaise "pcmanfm" (className =? "Pcmanfm"))
  , ("M-g v", runOrRaise "virt-manager" (className =? "Virt-manager"))
  , ("M-g s", spawn $ "slock")]
