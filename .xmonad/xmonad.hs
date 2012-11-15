import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig
import XMonad.Util.Run

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive

import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
-- Hooks
import XMonad.Operations
 
import System.IO
import System.Exit
 
import XMonad.Util.Run
 
 
import XMonad.Actions.CycleWS
 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
 
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
 
import Data.Ratio ((%))
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M


--http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html
--http://thinkingeek.com/2011/11/21/simple-guide-configure-xmonad-dzen2-conky/

myDzenBar = "dzen2 -w '1920' -ta 'l'" ++ myStatusBarStyle
myConkyBar = "conky -c ~/.xmonad/conkyrc | dzen2 -x '1920' -w '1920' -ta 'r'" ++ myStatusBarStyle
myStatusBarStyle = " -h '24' -fg '#fdf6e3' -bg '#002b36'"

main = do
    dzenLeftBar <- spawnPipe myDzenBar
    dzenRightBar <- spawnPipe myConkyBar
    xmonad $ gnomeConfig {
		modMask = mod4Mask -- mod key = win
		, workspaces          = [ "1:emacs", "2:web", "3:mail", "4:trello", "5:shell", "6:dev", "7:tunes" ]
		, logHook             = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
      
	}
	`additionalKeysP`
	 [
			("M-<KP_Subtract>", spawn "amixer -q -c 0 set Master 2+ unmute"),
			("M-<KP_Add>", spawn "amixer -q -c 0 set Master 2- unmute"),
			("M-<KP_Multiply>", spawn "amixer -q sset Master toggle")
     ]

--Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#2aa198" "#002b36" . pad
      , ppVisible           =   dzenColor "#859900" "#002b36" . pad
      , ppHidden            =   dzenColor "#fdf6e3" "#002b36" . pad
      , ppUrgent            =   dzenColor "#fdf6e3" "#002b36" . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppTitle             =   (" " ++) . dzenColor "#fdf6e3" "#002b36" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
