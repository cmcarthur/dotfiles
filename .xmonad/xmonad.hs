import XMonad
import XMonad.Config.Gnome
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import System.IO

myDzenBar = "dzen2 -w '1920' -ta 'l'" ++ myStatusBarStyle
myConkyBar = "conky -c ~/.xmonad/conkyrc | dzen2 -x '1920' -w '1920' -ta 'r'" ++ myStatusBarStyle
myStatusBarStyle = " -h '24' -fg '#D0D0D0' -bg '#151515' -fn 'Source Code Pro'"

main = do
    dzenLeftBar <- spawnPipe myDzenBar
    dzenRightBar <- spawnPipe myConkyBar
    xmonad $ gnomeConfig {
		modMask = mod4Mask -- mod key = win
		, workspaces          = [ "1:local", "2:vm", "3:prod", "4:browser", "5:personal" ]
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
        ppCurrent           =   dzenColor "#7E8D50" "#151515" . pad
      , ppVisible           =   dzenColor "#E5B566" "#151515" . pad
      , ppHidden            =   dzenColor "#6C99BA" "#151515" . pad
      , ppUrgent            =   dzenColor "#6C99BA" "#151515" . pad
      , ppWsSep             =   ""
      , ppSep               =   " | "
      , ppTitle             =   ("" ++) . dzenColor "#D0D0D0" "#151515" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
