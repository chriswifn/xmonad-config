module Custom.MyKeys where

-- main libraries
import XMonad
import qualified XMonad.StackSet as W

-- some other necessary imports
-- exit
import System.Exit
-- cycle and move betweens monitors
import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen)
-- copy
import XMonad.Actions.CopyWindow
-- eazy keybindings
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)
-- toggle fullscreen
import XMonad.Layout.ToggleLayouts (ToggleLayout (Toggle))
-- for some themes
import Custom.MyVariables
-- Treeselect
import XMonad.Actions.TreeSelect
-- switch between applications in grid select
import XMonad.Actions.GridSelect

-- Keymaps
myKeys = \c -> mkKeymap c $
  -- ESSENTIAL KEYBINDINGS 

  -- spawn a terminal
  [ ("M-S-<Return>", spawn $ terminal c)

  -- spawn a run launcher (dmenu)
  , ("M-S-p", spawn $ "dmenu_run -l 10 -p 'Application: '")

  -- kill a window
  , ("M-S-c", kill)

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
  , ("M-f", sendMessage $ Toggle "Full")

  -- focus on next screen
  , ("M-.", nextScreen)

  -- focus on previos screen
  , ("M-,", prevScreen)

  -- shift client to next screen and focus on it
  , ("M-S-.", shiftNextScreen >> nextScreen)

  -- shift client to previous screen and focus on it
  , ("M-S-,", shiftPrevScreen >> prevScreen)

  -- increment the amount of master nodes
  , ("M-d", sendMessage (IncMasterN 1))

  -- decrement the amount of master nodes
  , ("M-u", sendMessage (IncMasterN (-1)))

  -- recompile and restart XMonad
  , ("M-q", spawn $ "xmonad --recompile; xmonad --restart")

  -- quit XMonad
  , ("M-S-q", io (exitWith ExitSuccess))]
  ++

  -- TREESELECT AND GRID (for workspaces and a custom menu)
  -- custom menu
  [ ("M-;", Custom.MyVariables.treeselectAction myTSConfig)

  -- shift client to selected workspace and shift focus to that client
  , ("M-b s", treeselectWorkspace myTSConfig myWorkspaces (\ws -> W.view ws . W.shift ws))

  -- go to selected workspace and switch focus
  , ("M-b g", treeselectWorkspace myTSConfig myWorkspaces W.greedyView)

  -- show all programs in grid and switches focus to the selected one
  , ("M-b b", goToSelected $ mygridConfig Custom.MyVariables.myColorizer)

  -- shows all programs in grid and kills the selected one 
  , ("M-b c", withSelectedWindow killWindow (mygridConfig Custom.MyVariables.myColorizer))
  ]
  ++

  -- EMACS PROGRAMS
  [ ("M-e e", spawn $ myEmacs)
  , ("M-e b", spawn $ myEmacs ++ ("--eval '(ibuffer)'"))
  , ("M-e d", spawn $ myEmacs ++ ("--eval '(dired nil)'"))
  , ("M-e t", spawn $ myEmacs ++ ("--eval '(+vterm/here nil)'"))
  , ("M-e m", spawn $ myEmacs ++ ("--eval '(emms-smart-browse)'"))]
  ++

  -- TERMINAL PROGRAMS
  [ ("M-t t", spawn $ myTerminal ++ ("-c 'dev' -e tmux"))
  , ("M-t n", spawn $ myTerminal ++ ("-c 'nvim' -e nvim"))
  , ("M-t h", spawn $ myTerminal ++ ("-c 'htop' -e htop"))
  , ("M-t a", spawn $ myTerminal ++ ("-c 'mus' -e cmus"))
  , ("M-t r", spawn $ myTerminal ++ ("-c 'file' -e ranger"))
  , ("M-t p", spawn $ myTerminal ++ ("-c 'pulse' -e pulsemixer"))
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
  [ ("M-g g", spawn $ "firefox")
  , ("M-g z", spawn $ "zathura")
  , ("M-g f", spawn $ "pcmanfm")
  , ("M-g v", spawn $ "virt-manager")
  , ("M-g s", spawn $ "slock")]
