module Custom.MySwallow where

-- import base
import XMonad

-- import window swallowing
import XMonad.Hooks.WindowSwallowing

myHandleEventHook = swallowEventHook (className =? "Alacritty" <||> className =? "st-256color") (return True)
