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
import XMonad.Actions.DynamicWorkspaceGroups

import XMonad.Actions.EasyMotion (selectWindow)

-- prompts for open windows and workspaces
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.RunOrRaise

-- Keymaps
myKeys = \c -> mkKeymap c $
  -- ESSENTIAL KEYBINDINGS 

  -- spawn a terminal
  [ ("M-S-<Return>", spawn $ terminal c)

  , ("M-<Return>", raiseNextMaybe (spawn "st") (className =? "st-256color" <||> title =? "tmux" <||> title =? "devtmux"))

  -- spawn a run launcher (dmenu)
  , ("M-S-p", spawn $ "dmenu_run -l 10 -p 'Application: '")

  -- kill a window
  , ("M-S-c", kill1)

  -- switch between layouts
  , ("M-<Space>", sendMessage NextLayout)

  -- set a layout
  , ("M-S-<Space>", setLayout $ XMonad.layoutHook c)

  -- refresh
  , ("M-n", refresh)

  -- make a client sticky
  , ("M-s", windows copyToAll)

  -- make a client unsticky
  , ("M-S-s", killAllOtherCopies)

  -- focus between windows one way
  , ("M-<Tab>", windows W.focusDown)

  -- focus between windows another way
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)

  -- switch focus to the master client
  , ("M-m", windows W.focusMaster)

  -- switch a client in the stack with the master client
  , ("M-S-m", windows W.swapMaster)

  -- swap windows up and down
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)

  -- make client smaller
  , ("M-h", sendMessage Shrink)

  -- make client bigger
  , ("M-l", sendMessage Expand)

  -- force floating client back to tiling
  , ("M-S-t", withFocused $ windows . W.sink)

  -- make a client fullscreen
  , ("M-S-f", sendMessage $ Toggle "Full")

  -- increment the amount of master nodes
  , ("M-d", sendMessage (IncMasterN 1))

  -- decrement the amount of master nodes
  , ("M-u", sendMessage (IncMasterN (-1)))

  -- recompile and restart XMonad
  , ("M-q", spawn $ "xmonad --recompile; xmonad --restart")

  -- quit XMonad
  , ("M-S-q", io (exitWith ExitSuccess))]
  ++

  -- STUFF FOR DYNAMIC PROJECTS (mainly prompts)
  [ ("M-b b", windowPrompt myXPConfig Goto allWindows)

  , ("M-b v", windowPrompt myXPConfig BringCopy allWindows)

  , ("M-b g", workspacePrompt myXPConfig (windows . W.view))

  , ("M-b c", workspacePrompt myXPConfig (windows . copy))

  , ("M-b f", selectWindow myemConf >>=  (`whenJust` windows . W.focusWindow))

  , ("M-b k", selectWindow myemkillConf >>=  (`whenJust` killWindow))

  , ("M-b p", switchProjectPrompt myXPConfig)

  , ("M-b r", renameProjectPrompt myXPConfig)

  , ("M-b d", changeProjectDirPrompt myXPConfig)

  , ("M-b s", shiftToProjectPrompt myXPConfig)

  , ("M-b t", toggleWS)

  , ("M-b l", runOrRaisePrompt myXPConfig)

  ]
  ++

  -- DYNAMIC WORKSPACE GROUPS
  [ ("M-f n", promptWSGroupAdd myXPConfig "Name this group: ")
  , ("M-f g", promptWSGroupView myXPConfig "Go to group: ")
  , ("M-f d", promptWSGroupForget myXPConfig "Forget group: ")
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

  -- EMACS PROGRAMS
  [ ("M-e e", raiseNextMaybe (spawn myEmacs) (className =? "Emacs"))
  , ("M-e b", spawn $ myEmacs ++ ("--eval '(ibuffer)'"))
  , ("M-e d", spawn $ myEmacs ++ ("--eval '(dired nil)'"))
  , ("M-e t", spawn $ myEmacs ++ ("--eval '(+vterm/here nil)'"))
  , ("M-e m", spawn $ myEmacs ++ ("--eval '(emms-smart-browse)'"))]
  ++

  -- TERMINAL PROGRAMS
  [ ("M-t t", raiseMaybe (runInTerm "-T tmux" "tmux") (title =? "tmux"))
  , ("M-t n", raiseMaybe (runInTerm "-T nvim" "nvim") (title =? "nvim"))
  , ("M-t h", raiseMaybe (runInTerm "-T htop" "htop") (title =? "htop"))
  , ("M-t a", raiseMaybe (runInTerm "-T cmus" "cmus") (title =? "cmus"))
  , ("M-t r", raiseMaybe (runInTerm "-T lf" "lf-run") (title =? "lf"))
  , ("M-t p", raiseMaybe (runInTerm "-T pulsemixer" "pulsemixer") (title =? "pulsemixer"))
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
  [ ("M-p a", spawn $ "dmenu_run -l 10 -p 'Application: '")
  , ("M-p m", spawn $ "monitors")
  , ("M-p b", spawn $ "bookmarks")
  , ("M-p k", spawn $ "keyboard")
  , ("M-p s", spawn $ "maimmenu")
  , ("M-p i", spawn $ "network")
  , ("M-p l", spawn $ "logoutmenu")
  , ("M-p p", spawn $ "passmenu -l 20 -p 'Password: '")
  , ("M-p w", spawn $ "connectwifi")
  , ("M-p e", spawn $ "emojipicker")
  , ("M-p v", spawn $ "audiodevice")
  , ("M-p c", spawn $ "audioinputdevice")
  , ("M-p t", spawn $ "touchpad")
  ]
  ++

  -- GUI PROGRAMS
  [ ("M-g g", raiseNextMaybe (spawn "firefox") (className =? "firefox"))
  , ("M-g b", spawn "firefox")
  , ("M-g z", runOrRaise "zathura" (className =? "Zathura"))
  , ("M-g f", runOrRaise "pcmanfm" (className =? "Pcmanfm"))
  , ("M-g v", runOrRaise "virt-manager" (className =? "Virt-manager"))
  , ("M-g s", spawn $ "slock")]
