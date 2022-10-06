module Custom.MyKeys where

-- main libraries
import XMonad
import qualified XMonad.StackSet as W

-- some other necessary imports
import System.Exit
import XMonad.Actions.CycleWS (nextScreen, prevScreen)
import XMonad.Actions.CopyWindow
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)
import Custom.MyVariables

-- Keymaps
myKeys = \c -> mkKeymap c $
  -- ESSENTIAL KEYBINDINGS 

  -- spawn a terminal
  [ ("M-S-<Return>", spawn $ terminal c)

  -- spawn a run launcher (dmenu)
  , ("M-S-p", spawn $ "dmenu_run -l 20")

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

  -- focus on next screen
  , ("M-.", nextScreen)

  -- focus on previos screen
  , ("M-,", prevScreen)

  -- increment the amount of master nodes
  , ("M-d", sendMessage (IncMasterN 1))

  -- decrement the amount of master nodes
  , ("M-u", sendMessage (IncMasterN (-1)))

  -- recompile and restart XMonad
  , ("M-q", spawn $ "xmonad --recompile; xmonad --restart")

  -- quit XMonad
  , ("M-S-q", io (exitWith ExitSuccess))]
  ++

  -- SEND CLIENT TO WORKSPACE AND SWITCH WORKSPACE
  [ (otherModMasks ++ "M-" ++ key, action tag)
      | (tag, key)  <- zip myWorkspaces (map (\x -> "" ++ show x ++ "") [1..9])
      , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
                                   , ("S-", windows . W.shift)]
  ]
  ++

  -- EMACS PROGRAMS
  [ ("M-e e", spawn $ myEmacs)
  , ("M-e b", spawn $ myEmacs ++ ("--eval '(ibuffer)'"))
  , ("M-e d", spawn $ myEmacs ++ ("--eval '(dired nil)'"))
  , ("M-e t", spawn $ myEmacs ++ ("--eval '(+vterm/here nil)'"))]
  ++

  -- TERMINAL PROGRAMS
  [ ("M-t t", spawn $ myTerminal ++ ("-c 'dev' -e tmux"))
  , ("M-t n", spawn $ myTerminal ++ ("-c 'nvim' -e nvim"))
  , ("M-t h", spawn $ myTerminal ++ ("-e htop"))
  , ("M-t a", spawn $ myTerminal ++ ("-c 'mus' -e cmus"))
  , ("M-t r", spawn $ myTerminal ++ ("-c 'file' -e lf-run"))
  , ("M-t p", spawn $ myTerminal ++ ("-e pulsemixer"))
  ]
  ++

  -- DMENU SCRIPTS
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

  -- GUI PROGRAMS
  [ ("M-g", spawn $ "firefox")
  , ("M-z", spawn $ "zathura")
  , ("M-S-f", spawn $ "pcmanfm")
  , ("M-v", spawn $ "virt-manager")
  , ("M-S-C-s", spawn $ "slock")]
  ++

  -- SCRIPTS TO CONTROL AUDIO (both input and output), BRIGHTNESS AND A NIGHT MODE
  [ ("M-<F1>", spawn $ "volume mute")
  , ("M-<F2>", spawn $ "volume down")
  , ("M-<F3>", spawn $ "volume up")
  , ("M-<F4>", spawn $ "microphone mute")
  , ("M-<F5>", spawn $ "microphone down")
  , ("M-<F6>", spawn $ "microphone up")
  , ("M-<F7>", spawn $ "brightness down")
  , ("M-<F8>", spawn $ "brightness up")
  , ("M-<F9>", spawn $ "gamma")]
