import XMonad
import XMonad.Config.Gnome
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import System.IO

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
			("M-<KP_Add>", spawn "amixer -q -c 0 set Master 2- unmute")
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
