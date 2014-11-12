import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS

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
        , modMask = mod4Mask 
        } 
        `removeKeysP` 
        ["M4-" ++ n | n <- unwantedKeys ]
        `additionalKeysP` 
        addedKeys
        

myTerminal = "xterm"

myWorkspaces = ["1:main" 
               ,"2:web" 
               ,"3:emacs" 
               ,"4:term" 
               ,"5:media"]

myFocusedBorderColor = "#85E0FF"

unwantedKeys = ["p"  -- You guys should be ashamed of yourselves
               ,"<Tab>" 
               ,"Return"
               ]

addedKeys = [("M4-r", spawn "dmenu_run")
            ,("M4-n", nextWS)
            ,("M4-p", prevWS)
            ,("M4-<Tab>", toggleWS)
            ]

startup :: X ()
startup = do
  spawn "xterm"
