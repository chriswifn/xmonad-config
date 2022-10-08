module Custom.MyAutostart where

-- import main package
import XMonad
-- to autostart applications
import XMonad.Util.SpawnOnce
-- for some java applications that don't play well with window managers
import XMonad.Hooks.SetWMName

-- start some applications on launch and set the WMName
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "setxkbmap -option caps:escape"
  spawnOnce "/usr/bin/lxpolkit"
  spawnOnce "dunst"
  spawnOnce "/usr/bin/emacs --daemon"
  setWMName "LG3D"
