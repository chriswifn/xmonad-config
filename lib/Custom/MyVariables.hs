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

-- Colorscheme
import Colors.ModusVivendi

-- st is objectively the best terminal
myTerminal :: String
myTerminal = "alacritty "

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
  , borderColor          = color06
  , bgColor              = colorBack 
  , fgColor              = colorFore 
  , bgHLight             = color06 
  , fgHLight             = colorBack 
  , defaultText          = "" 
  , font                 = myFont 
  , height               = 20
  , promptBorderWidth    = 2
  , maxComplColumns      = Just 1
  , maxComplRows         = Just 10
  , complCaseSensitivity = CaseInSensitive
  , promptKeymap         = emacsLikeXPKeymap
  -- , completionKey        = (controlMask, xK_n)
  -- , historySize          = 0
  }

-- border width
myBorderWidth :: Dimension 
myBorderWidth = 2

-- black is my primary background color, inactive border should blend into the background
myNormalBorderColor :: String
myNormalBorderColor = colorBack 

-- a nice color that fints the theme
myFocusedBorderColor :: String
myFocusedBorderColor = color06 

myFont :: String
myFont = "xft:Monoid:style=Regular:size=9:antialias=true:hinting=true"
-- myFont = "xft:Terminus:style=Regular:size=12:antialias=true:hinting=true"
-- myFont = "xft:Iosevke Comfy Wide Fixed:style=Regular:size=11:antialias=true:hinting=true"

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
myemConf = def { txtCol = colorFore 
                 , bgCol = colorBack 
                 , borderCol = color06 
                 , cancelKey = xK_Escape
                 , overlayF = proportional 0.05
                 , emFont = "xft:Monoid:style=Regular:size=20:antialias=true:hinting=true"
                 -- , emFont = "xft:Iosevka Comfy Wide Fixed:style=Regular:size=20:antialias=true:hinting=true"
                 -- , emFont = "xft:Terminus:style=Regular:size=20:antialias=true:hinting=true"
                 , borderPx = 1
               }

myemkillConf :: EasyMotionConfig
myemkillConf = def { txtCol = color02 
                   , bgCol = colorBack
                   , borderCol = color06 
                   , cancelKey = xK_Escape
                   , overlayF = proportional 0.05
                   , emFont = "xft:Monoid:style=Regular:size=20:antialias=true:hinting=true"
                   -- , emFont = "xft:Iosevka Comfy Wide Fixed:style=Regular:size=20:antialias=true:hinting=true"
                   -- , emFont = "xft:Terminus:style=Regular:size=20:antialias=true:hinting=true"
                   , borderPx = 1
                   }


