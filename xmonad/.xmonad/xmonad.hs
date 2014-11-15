
import XMonad
import System.IO

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FloatNext

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig

import qualified XMonad.Actions.Search as S
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.Volume

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders

import XMonad.Prompt

myTerminal = "xterm"
myBrowser = "uzbl-tabbed"

myHighlight = "#00FF00"
myLowlight = "#00008A"
myEmpty = "#999999"
myVisible = "#FFFF00"
myForeground = "#FFFFFF"
myBackground = "#000000"
myAccent = "#85E0FF"

myBar = "xmobar /home/dallas/.xmobarrc"
myBorderWidth = 2
myWorkspaces = ["1:main" 
               ,"2:web" 
               ,"3:emacs" 
               ,"4:term" 
               ,"5:media"
               ]

main = do
    h <- spawnPipe myBar -- used in logHook 
    xmonad $ defaultConfig
        { terminal = myTerminal 
        , workspaces = myWorkspaces
        , focusedBorderColor = myAccent
        , borderWidth = myBorderWidth
        , manageHook =  manageHook defaultConfig <+> manageDocks <+> myManageHook
        , layoutHook = avoidStruts $ 
                       smartBorders $ 
                       spacing 2 $ 
                       layoutHook defaultConfig
        , startupHook = myStartup
        , logHook = myLogHook h
        , modMask = mod4Mask 
        } 
        `removeKeysP` ["M4-" ++ n | n <- unwantedKeys ]
        `additionalKeysP` addedKeys

myStartup = do
    spawn "emacs --daemon"
    spawn "xmodmap ~/.xmodmap"
    spawn "xscreensaver -no-splash &"
    spawn "xterm"

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
            ,("M4-w g", gotoMenu)
            ,("M4-w b", bringMenu)
            ,("M4-<F1>", runOrRaise "emacsclient -c -a emacs " (className =? "Emacs"))
            ,("M4-<F2>", runOrRaise "uzbl-tabbed" (className =? "Uzbl-tabbed"))
            ,("M4-<F12>", spawn "xterm -e alsamixer")
            ,("M4-S-<F12>", spawn "amixer -D pulse set Master toggle")
            ] 
            -- Search functionality (thanks tylevad on Github!)
            ++ [("M4-s " ++ k, S.promptSearchBrowser myXPConfig myBrowser f) | (k,f) <- searchEngines]
               where searchEngines = [ ("g", S.google)
                                     , ("d", S.searchEngine "DuckDuckGo" "https://duckduckgo.com/?q=")
                                     , ("w", S.searchEngine "Wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=")
                                     , ("y", S.searchEngine "YouTube" "https://www.youtube.com/results?search_query=")
                                     , ("a", S.searchEngine "ArchWiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
                                     ]

myManageHook = composeAll [
                          ]

myLogHook h = (dynamicLogWithPP $ myPP h)

myPP h = xmobarPP
  { ppCurrent         = xmobarColor myEmpty "" . wrap "{" "}" . xmobarColor myHighlight ""
  , ppVisible         = xmobarColor myEmpty "" . wrap "[" "]" . xmobarColor myVisible ""
  , ppHidden          = xmobarColor myForeground ""
  , ppHiddenNoWindows = xmobarColor myEmpty ""
  , ppTitle           = xmobarColor myForeground "" . shorten 130
  , ppLayout          = xmobarColor myAccent ""
  , ppSep             = " <fc=" ++ myLowlight ++ ">|</fc> "
  , ppWsSep           = " "
  , ppOutput          = hPutStrLn h
  }

myXPConfig = defaultXPConfig
  { fgColor = myForeground
  , bgColor = myBackground
  , bgHLight = myBackground
  , fgHLight = myAccent
  , borderColor = myAccent
  , position = Bottom
  , historySize = 0
  , height = 16
  }


