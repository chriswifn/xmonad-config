module Custom.MyVariables where

-- main library
import XMonad
-- tabbed layout
import XMonad.Layout.Tabbed
-- prompts
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch

import XMonad.Actions.DynamicProjects

import XMonad.Actions.EasyMotion (EasyMotionConfig (..), fixedSize)

-- st is objectively the best terminal
myTerminal :: String
myTerminal = "st "

-- emacs is objectively the best text editor
myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "

myBrowser :: String
myBrowser = "firefox"

-- Alt is a stupid modifier key, so use Super
myModMask :: KeyMask
myModMask = mod4Mask

-- sometimes useful
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- sometimes useful
myClickJustFocuses :: Bool
myClickJustFocuses = False 

myXPConfig = def
  { position             = Bottom
  , searchPredicate      = fuzzyMatch
  , sorter               = fuzzySort
  , alwaysHighlight      = True 
  , borderColor          = "#ccdfe7"
  , bgColor              = "#1e1e1e" 
  , fgColor              = "#ffffff" 
  , bgHLight             = "#f78fe7" 
  , fgHLight             = "#000000" 
  , defaultText          = ""
  , font                 = myFont 
  , height               = 20
  , promptBorderWidth    = 1
  , maxComplColumns      = Just 1
  , maxComplRows         = Just 10
  , complCaseSensitivity = CaseInSensitive
  , promptKeymap         = emacsLikeXPKeymap
  , historySize          = 0
  }

-- border width
myBorderWidth :: Dimension 
myBorderWidth = 1

-- black is my primary background color, inactive border should blend into the background
myNormalBorderColor :: String
myNormalBorderColor = "#323232"

-- a nice color that fints the theme
myFocusedBorderColor :: String
myFocusedBorderColor = "#f78fe7"

myFont :: String
myFont = "xft:Terminus (TTF):regular:size=12:antialias=true:hinting=true"

myTabConfig = def { fontName = myFont 
                  , activeColor = "#323232"
                  , inactiveColor = "#1e1e1e"
                  , activeBorderColor = "#323232"
                  , inactiveBorderColor = "#1e1e1e"
                  , activeTextColor = "#ccdfe7"
                  , inactiveTextColor = "#ccdfe7"
                  }

webWS :: String
webWS = "web"

emacsWS :: String
emacsWS = "emacs"

devWS :: String
devWS = "dev"

fileWS :: String
fileWS = "file"

officeWS :: String
officeWS = "office"

worktutWS :: String
worktutWS = "worktut"

worksysWS :: String
worksysWS = "worksys"

nullWS :: String
nullWS = "null"

temptermWS :: String
temptermWS = "tempterm"

fireWS :: String
fireWS = "firefox"


myWorkspaces :: [WorkspaceId]
myWorkspaces = [webWS, emacsWS, devWS, fileWS, officeWS, worktutWS, worksysWS, nullWS, temptermWS, fireWS]

projects :: [Project]
projects =
  [ Project { projectName      = webWS
            , projectDirectory = "~/"
            , projectStartHook = Nothing 
            }

  , Project { projectName      = emacsWS
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn myEmacs
            }

  , Project { projectName      = devWS
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn (myTerminal <> " -e tmux new-session -A -s dev")
            }

  , Project { projectName      = fileWS 
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn (myTerminal <> " -e lf-run")
            }

  , Project { projectName      = officeWS 
            , projectDirectory = "~/Documents"
            , projectStartHook = Just $ do spawn "libreoffice" 
            }

  , Project { projectName      = worktutWS 
            , projectDirectory = "~/Documents/Uni/num_prog"
            , projectStartHook = Just $ do spawn (myEmacs ++ ("--eval '(dired nil)'"))
                                           spawn (myTerminal <> " -e tmux new-session -A -s tut")
            }

  , Project { projectName      = worksysWS 
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn (myTerminal <> " -e tmux new-session -A -s sys") 
            }

  , Project { projectName      = nullWS 
            , projectDirectory = "~/"
            , projectStartHook = Nothing 
            }

  , Project { projectName      = temptermWS 
            , projectDirectory = "~/"
            , projectStartHook = Nothing 
            }

  , Project { projectName      = fireWS 
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "firefox" 
            }
  ]


myemConf :: EasyMotionConfig
myemConf = def { txtCol = "#44bc44"
                 , bgCol = "#1e1e1e"
                 , borderCol = "#ccdfe7"
                 , cancelKey = xK_Escape
                 , emFont = "xft:Terminus (TTF):regular:size=80:antialias=true:hinting=true"
                 , borderPx = 1
               }

myemkillConf :: EasyMotionConfig
myemkillConf = def { txtCol = "#ff8059"
                   , bgCol = "#1e1e1e"
                   , borderCol = "#ccdfe7"
                   , cancelKey = xK_Escape
                   , emFont = "xft:Terminus (TTF):regular:size=80:antialias=true:hinting=true"
                   , borderPx = 1
                   }
