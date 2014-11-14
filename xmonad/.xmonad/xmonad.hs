import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.ManageHook
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import qualified XMonad.Actions.Search as S
import XMonad.Prompt

main = do
    xmproc <- spawnPipe "xmobar /home/dallas/.xmobarrc"
    xmonad $ defaultConfig
        { terminal = myTerminal 
        , workspaces = myWorkspaces
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth = 2
        , manageHook =  manageHook defaultConfig <+> manageDocks <+> myManageHook
        , layoutHook = avoidStruts $ smartBorders $ spacing 2 $ layoutHook defaultConfig
        , startupHook = startup
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "purple" "" . shorten 50
                        }
        , modMask = mod4Mask 
        } 
        `removeKeysP` ["M4-" ++ n | n <- unwantedKeys ]
        `additionalKeysP` addedKeys
        
-- Find a better way to do this stuff, but for now, it works...
startup :: X ()
startup = do
    spawn "emacs --daemon"
    spawn "xmodmap ~/.xmodmap"
    spawn "xscreensaver -no-splash &"
    spawn "xterm"

myTerminal = "xterm"
myBrowser = "uzbl-tabbed"

myWorkspaces = ["1:main" 
               ,"2:web" 
               ,"3:emacs" 
               ,"4:term" 
               ,"5:media"
               ]

myFocusedBorderColor = "#85E0FF"

unwantedKeys = ["p"  -- You guys should be ashamed of yourselves
               ,"n"
               ,"<Tab>" 
               ,"<Return>"
               ]

addedKeys = [("M4-r", spawn "dmenu_run")
            ,("M4-=", refresh) -- why doesn't this work?
            ,("M4-n", nextWS)
            ,("M4-p", prevWS)
            ,("M4-<Tab>", toggleWS)
            ,("M4-s M4-l", spawn "xscreensaver-command --lock")
            ,("M4-s M4-s", spawn "scrot ~/Documents/screenshots/%Y-%m-%d-%T-screenshot.png")
            ,("M4-<Return>", spawn "xterm -e /home/dallas/scripts/screen.sh")
            ] 
            ++ [("M4-s M4-" ++ k, S.promptSearchBrowser defaultXPConfig myBrowser f) | (k,f) <- searchEngines]
               where searchEngines = [ ("g", S.google)
                                     , ("d", S.searchEngine "DuckDuckGo" "https://duckduckgo.com/?q=")
                                     , ("w", S.searchEngine "Wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=")
                                     , ("a", S.searchEngine "ArchWiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
                                     ]

myManageHook = composeAll [
               ]

