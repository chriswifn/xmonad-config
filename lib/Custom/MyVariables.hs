module Custom.MyVariables where

-- main library
import XMonad

-- st is objectively the best terminal
myTerminal :: String
myTerminal = "st "

-- emacs is objectively the best text editor
myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "

-- Alt is a stupid modifier key, so use Super
myModMask :: KeyMask
myModMask = mod4Mask

-- sometimes useful
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True 

-- sometimes useful
myClickJustFocuses :: Bool
myClickJustFocuses = True 

-- myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces    = ["1:WWW","2:EMACS","3:TERM","4:FILE","5:DEV","6:DOC","7:MUS","8:VID","9:VIRT"]

-- border widht: a nice big border
myBorderWidth :: Dimension 
myBorderWidth = 5

-- black is my primary background color, inactive border should blend into the background
myNormalBorderColor :: String
myNormalBorderColor = "#000000"

-- a nice color that fints the theme
myFocusedBorderColor :: String
myFocusedBorderColor = "#f78fe7"
