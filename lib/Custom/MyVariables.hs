module Custom.MyVariables where

-- main library
import XMonad
-- tabbed layout
import XMonad.Layout.Tabbed
-- prompts
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch

import XMonad.Actions.DynamicProjects

import XMonad.Actions.EasyMotion (EasyMotionConfig (..), proportional)

import XMonad.Util.Run

import XMonad.Util.NamedScratchpad
import XMonad.StackSet as W

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
  -- , completionKey        = (controlMask, xK_n)
  -- , historySize          = 0
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
myFont = "xft:Monoid:style=Regular:size=9:antialias=true:hinting=true"

myTabConfig = def { fontName = myFont 
                  , activeColor = "#323232"
                  , inactiveColor = "#1e1e1e"
                  , activeBorderColor = "#ccdfe7"
                  , inactiveBorderColor = "#1e1e1e"
                  , activeTextColor = "#ccdfe7"
                  , inactiveTextColor = "#ccdfe7"
                  }

webWS :: String
webWS = "web"

emacsWS :: String
emacsWS = "emacs"

termWS :: String
termWS = "tmux-session"

myWorkspaces :: [WorkspaceId]
myWorkspaces = [webWS, emacsWS, termWS] 

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

  , Project { projectName      = termWS 
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn (myTerminal <> " -e tmux new-session -A -s tempterm")  
            }
  ]


myemConf :: EasyMotionConfig
myemConf = def { txtCol = "#ccdfe7"
                 , bgCol = "#1e1e1e"
                 , borderCol = "#ccdfe7"
                 , cancelKey = xK_Escape
                 , overlayF = proportional 0.05
                 , emFont = "xft:Monoid:style=Regular:size=20:antialias=true:hinting=true"
                 , borderPx = 1
               }

myemkillConf :: EasyMotionConfig
myemkillConf = def { txtCol = "#ff8059"
                   , bgCol = "#1e1e1e"
                   , borderCol = "#ccdfe7"
                   , cancelKey = xK_Escape
                   , overlayF = proportional 0.05
                   , emFont = "xft:Monoid:style=Regular:size=20:antialias=true:hinting=true"
                   , borderPx = 1
                   }

scratchpads = [ NS "htop" "st -c 'htop' -e htop" (className =? "htop") manageTerm 
              , NS "pulsemixer" "st -c 'pulsemixer' -e pulsemixer" (className =? "pulsemixer") manageTerm 
              ]
  where
    manageTerm = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)


