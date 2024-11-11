import XMonad
import System.IO

import XMonad.Config.Xfce
import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal = "termite"

myHighlight  = "#95e454"
myLowlight   = "#8ac6f2"
myEmpty      = "#999999"
myVisible    = "#e5786d"
myForeground = "#f6f3e8"
myBackground = "#242424"
myAccent     = "#85E0FF"

main = do
  h <- spawnPipe "xmobar /home/wzrd/.xmobarrc"
  xmonad $ xfceConfig
         { terminal           = myTerminal
         , borderWidth        = 2
         , focusedBorderColor = myHighlight
         , layoutHook         = avoidStruts $ smartBorders $ spacing 2 $ layoutHook defaultConfig
         , manageHook         = myManageHook <+> manageHook defaultConfig
         , logHook            = myLogHook h
         , modMask            = mod4Mask
         , startupHook        = myStartup
         } `additionalKeysP` myKeys

myManageHook = composeAll
      [ className =? "Xfrun4" --> doCenterFloat
      , manageDocks
      ]

myLogHook h = dynamicLogWithPP $ myPP h

myPP h = xmobarPP
  { ppCurrent         = xmobarColor myEmpty "" . wrap "{" "}" . xmobarColor myHighlight ""
  , ppVisible         = xmobarColor myEmpty "" . wrap "[" "]" . xmobarColor myVisible ""
  , ppHidden          = xmobarColor myForeground ""
  , ppHiddenNoWindows = xmobarColor myEmpty ""
  , ppTitle           = xmobarColor myForeground "" . shorten 100
  , ppLayout          = xmobarColor myForeground ""
  , ppSep             = " <fc=" ++ myLowlight ++ ">|</fc> "
  , ppWsSep           = " "
  , ppOutput          = hPutStrLn h
  }

myKeys =
  -- bindings for starting programs via the xfce finder
  [ ("M4-<Space>"     , spawn "xfrun4")
  , ("M4-S-<Space>"   , spawn "xfce4-appfinder")

  -- quitting / restarting / screen locking
  , ("M4-r"           , spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  , ("M4-q"           , spawn "xflock4")
  , ("M4-S-q"         , spawn "xfce4-session-logout")

  -- workspace / screen movement
  , ("M4-n"           , moveTo Next HiddenWS)
  , ("M4-S-n"         , moveTo Prev HiddenWS)
  , ("M4-h"           , onPrevNeighbour W.view)
  , ("M4-S-h"         , onPrevNeighbour W.shift)
  , ("M4-l"           , onNextNeighbour W.view)
  , ("M4-S-l"         , onNextNeighbour W.shift)
  , ("M4-<Tab>"       , toggleWS)

  -- window sizing
  , ("M4--"           , sendMessage Expand)
  , ("M4-S--"         , sendMessage Shrink)

  -- spawn tmux'd terminal or regular one
  , ("M4-<Return>"    , spawn "termite -e \"$SHELL -c 'tmux new-session -A -s $USER'\"")
  , ("M4-S-<Return>"  , spawn $ myTerminal)

  -- screenshots
  , ("M4-p"           , spawn "scrot ~/screenshots/screen-%Y-%m-%d.png")
  , ("M4-S-p"         , spawn "scrot ~/screenshots/window-%Y-%m-%d.png -u")

  -- killing stuff
  , ("M4-c"           , kill)
  , ("M4-S-c"         , spawn "xkill")
  ]

myStartup = spawn "xmodmap /home/wzrd/.xmodmap"

-- import XMonad
-- import System.IO
-- import qualified XMonad.StackSet as W

-- import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.ManageDocks

-- import XMonad.Util.Run(spawnPipe)
-- import XMonad.Util.EZConfig
-- import XMonad.Util.NamedScratchpad

-- import qualified XMonad.Actions.Search as S
-- import XMonad.Actions.CycleWS
-- import XMonad.Actions.WindowBringer
-- import XMonad.Actions.WindowGo
-- import XMonad.Actions.DynamicWorkspaces
-- import XMonad.Actions.DynamicWorkspaceGroups

-- import XMonad.Layout.Spacing
-- import XMonad.Layout.NoBorders

-- import XMonad.Prompt

-- import Control.Monad

-- myTerminal = "xterm"
-- myBrowser = "uzbl-tabbed"

-- myHighlight = "#00FF00"
-- myLowlight = "#00008A"
-- myEmpty = "#999999"
-- myVisible = "#FFFF00"
-- myForeground = "#FFFFFF"
-- myBackground = "#000000"
-- myAccent = "#85E0FF"

-- myBar = "xmobar /home/dallas/.xmobarrc"
-- myBorderWidth = 2
-- myWorkspaces = ["1" ,"2" ,"3" ,"4" ,"5"]

-- main = do
-- h <- spawnPipe myBar -- used in logHook
-- xmonad $ defaultConfig
-- 	  { terminal = myTerminal
-- 	  , workspaces = myWorkspaces
-- 	  , focusedBorderColor = myAccent
-- 	  , borderWidth = myBorderWidth
-- 	  , manageHook =  myManageHook
-- 	  , layoutHook = avoidStruts $ smartBorders $ spacing 2 $ layoutHook defaultConfig
-- 	  , startupHook = myStartup
-- 	  , logHook = myLogHook h
-- 	  , modMask = mod4Mask
-- 	  } `additionalKeysP` addedKeys

-- myScratchpads = [ NS "alsamixer" "xterm -e alsamixer" (title =? "alsamixer") (customFloating $ W.RationalRect 0.6 0.1 0.35 0.5)
--                 , NS "xterm" "xterm -name scratchterm" (title =? "scratchterm") (customFloating $ W.RationalRect 0.05 0.8 0.9 0.15)
--                 ]

-- toggleSkip :: [WorkspaceId] -> X ()
-- toggleSkip skips = do
--     hs <- gets (flip skipTags skips . W.hidden . windowset)
--     unless (null hs) (windows . W.view . W.tag $ head hs)

-- addedKeys = [("M4-r", spawn "dmenu_run")
-- ,("M4-n", nextWS)
-- ,("M4-p", prevWS)
-- ,("M4-`", gotoMenu)
-- ,("M4-<Tab>", toggleSkip ["NSP"])
-- ,("M4-s M4-l", spawn "xscreensaver-command --lock")
-- ,("M4-s M4-s", spawn "scrot ~/Documents/screenshots/%Y-%m-%d-%T-screenshot.png")
--             ,("M4-<F1>", runOrRaise "google-chrome" (className =? "Google-chrome"))
--             ,("M4-<F2>", runOrRaise "emacs" (className =? "Emacs"))
--             ,("M4-<F3>", runOrRaise "nautilus" (className =? "Nautilus"))
--             ,("M4-<F4>", runOrRaise "vlc" (className =? "vlc"))
--             ,("M4-<F12>", namedScratchpadAction myScratchpads "alsamixer")
--             ,("M4-S-<F12>", spawn "amixer -D pulse set Master toggle")
--             ,("M4-<Return>", namedScratchpadAction myScratchpads "xterm")
-- ,("M4-S-<Return>", spawn "xterm -e /home/dallas/scripts/screen.sh")
-- ]
-- -- Search functionality (thanks tylevad on Github!)
-- ++ [("M4-s " ++ k, S.promptSearchBrowser myXPConfig myBrowser f) | (k,f) <- searchEngines]
-- 	 where searchEngines = [("g", S.google)
-- 			 ,("d", S.searchEngine "DuckDuckGo" "https://duckduckgo.com/?q=")
-- 			 ,("w", S.searchEngine "Wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=")
-- 			 ,("y", S.searchEngine "YouTube" "https://www.youtube.com/results?search_query=")
-- 			 ,("a", S.searchEngine "ArchWiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
-- 			 ]

-- myManageHook = composeAll [ manageDocks
--                           , namedScratchpadManageHook myScratchpads
--                           ]

-- myLogHook h = (dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ myPP h)

-- myPP h = xmobarPP
--   { ppCurrent         = xmobarColor myEmpty "" . wrap "{" "}" . xmobarColor myHighlight ""
--   , ppVisible         = xmobarColor myEmpty "" . wrap "[" "]" . xmobarColor myVisible ""
--   , ppHidden          = xmobarColor myForeground ""
--   , ppHiddenNoWindows = xmobarColor myEmpty ""
--   , ppTitle           = xmobarColor myForeground "" . shorten 100
--   , ppLayout          = xmobarColor myAccent ""
--   , ppSep             = " <fc=" ++ myLowlight ++ ">|</fc> "
--   , ppWsSep           = " "
--   , ppOutput          = hPutStrLn h
--   }

-- myXPConfig = defaultXPConfig
--   { fgColor = myForeground
--   , bgColor = myBackground
--   , bgHLight = myBackground
--   , fgHLight = myAccent
--   , borderColor = myAccent
--   , position = Bottom
--   , historySize = 0
--   , height = 16
--   }
