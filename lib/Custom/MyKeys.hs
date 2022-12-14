module Custom.MyKeys where

-- main libraries
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Run

-- some other necessary imports
-- exit
import System.Exit
-- cycle and move betweens monitors
import XMonad.Actions.CycleWS
-- copy
import XMonad.Actions.CopyWindow
-- eazy keybindings
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)
-- toggle fullscreen
import XMonad.Layout.ToggleLayouts (ToggleLayout (Toggle))
-- for some themes
import Custom.MyVariables
-- Treeselect
import qualified XMonad.Actions.TreeSelect as TS
-- run or raise
import XMonad.Actions.WindowGo

-- prompts for open windows and workspaces
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.Pass
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell (prompt)
import qualified XMonad.Actions.Search as S

archwiki :: S.SearchEngine
archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="

searchList :: [(String, S.SearchEngine)]
searchList = [ ("g", S.google)
             , ("d", S.duckduckgo)
             , ("w", S.wikipedia)
             , ("s", S.amazon)
             , ("a", archwiki)
             , ("y", S.youtube)
             ]

-- Keymaps
myKeys = \c -> mkKeymap c $
  -- ESSENTIAL KEYBINDINGS 

  -- spawn a terminal
  [ ("M-S-<Return>", spawn $ terminal c)

  , ("M-<Return>", raiseNextMaybe (spawn "st") (className =? "st-256color"))

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

  -- TREESELECT AND PROMPT (for workspaces and a custom menu)
  -- MOSTLY FOR WORKSPACE STUFF
  -- custom menu
  [ ("M-;", Custom.MyVariables.treeselectAction myTSConfig)

  -- shift client to selected workspace and shift focus to that client
  , ("M-b s", TS.treeselectWorkspace myTSConfig myWorkspaces (\ws -> W.view ws . W.shift ws))

  -- go to selected workspace and switch focus
  , ("M-b g", TS.treeselectWorkspace myTSConfig myWorkspaces W.greedyView)

  -- show all programs in grid and switches focus to the selected one
  , ("M-b b", windowPrompt myXPConfig Goto allWindows)

  , ("M-b c", TS.treeselectWorkspace myTSConfig myWorkspaces (\ws -> copy ws))

  , ("M-b w", workspacePrompt myXPConfig (windows . W.view))

  , ("M-b t", toggleWS)

  , ("M-b n", moveTo Next (wsTagGroup '.'))

  , ("M-b p", moveTo Prev (wsTagGroup '.'))

  , ("M-b l", runOrRaisePrompt myXPConfig)

  , ("M-v c", passPrompt myXPConfig)

  , ("M-v n", passGeneratePrompt myXPConfig)

  , ("M-v t", passTypePrompt myXPConfig)

  , ("M-v e", passEditPrompt myXPConfig)

  , ("M-v r", passRemovePrompt myXPConfig)
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
  , ("M-t f", prompt ("st" ++ " -e") myXPConfig)
  ]
  ++

  -- Search shit in browser using mutiple search engines
  [ ("M-f " ++ k, S.promptSearchBrowser myXPConfig "firefox" f) | (k,f) <- searchList]
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
