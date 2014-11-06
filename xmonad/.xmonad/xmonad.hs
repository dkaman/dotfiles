import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    xmproc <- spawnPipe "xmobar /home/dallas/.xmobarrc"
    xmonad $ defaultConfig
        { terminal = myTerminal 
        , workspaces = myWorkspaces
        , focusedBorderColor = myFocusedBorderColor
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , startupHook = startup
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "purple" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]

myTerminal = "xterm"
myWorkspaces = ["1:main", "2:web", "3:emacs", "4:term", "5:media"]
myFocusedBorderColor = "#85E0FF"

startup :: X ()
startup = do
  spawn "xterm"
  spawn "emacs --daemon"
  spawn "xmodmap ~/.xmodmap"
