import XMonad
import System.IO

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders

import Data.List (isPrefixOf)
import System.Directory (getHomeDirectory, doesFileExist)

import qualified XMonad.StackSet as W

myTerminal = "termite"

myHighlight  = "#95e454"
myLowlight   = "#8ac6f2"
myEmpty      = "#999999"
myVisible    = "#e5786d"
myForeground = "#f6f3e8"
-- myBackground = "#242424"
-- myAccent     = "#85E0FF"

main = do
  xmonad $ withEasySB mySB defToggleStrutsKey $ ewmhFullscreen $ ewmh $ def
         { terminal           = myTerminal
         , borderWidth        = 2
         , focusedBorderColor = myHighlight
         , layoutHook         = avoidStruts $ smartBorders $ spacing 2 $ layoutHook def
         , manageHook         = myManageHook <+> manageHook def
         , modMask            = mod4Mask
         } `additionalKeysP` myKeys

mySB = statusBarProp "xmobar ~/.xmobarrc" (pure myPP)

myManageHook = composeAll
      [ className =? "Xfrun4" --> doCenterFloat
      , title =? "firefox profile manager" --> doCenterFloat
      , isDialog --> doFloat
      , manageDocks
      ]

myPP = def
  { ppCurrent         = xmobarColor myEmpty "" . wrap "{" "}" . xmobarColor myHighlight ""
  , ppVisible         = xmobarColor myEmpty "" . wrap "[" "]" . xmobarColor myVisible ""
  , ppHidden          = xmobarColor myForeground ""
  , ppHiddenNoWindows = xmobarColor myEmpty ""
  , ppTitle           = xmobarColor myForeground "" . shorten 100
  , ppLayout          = xmobarColor myForeground ""
  , ppSep             = " <fc=" ++ myLowlight ++ ">|</fc> "
  , ppWsSep           = " "
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
  , ("M4-n"           , moveTo Next hiddenWS)
  , ("M4-S-n"         , moveTo Prev hiddenWS)
  , ("M4-h"           , onPrevNeighbour def W.view)
  , ("M4-S-h"         , onPrevNeighbour def W.shift)
  , ("M4-l"           , onNextNeighbour def W.view)
  , ("M4-S-l"         , onNextNeighbour def W.shift)
  , ("M4-<Tab>"       , toggleWS)

  -- browser stuff
  , ("M-t"          , launchFirefoxWithProfile)

  -- window sizing
  , ("M4--"           , sendMessage Expand)
  , ("M4-S--"         , sendMessage Shrink)
  , ("M4-="           , spawn "autorandr --change")

  -- spawn tmux'd terminal or regular one
  , ("M4-<Return>"    , spawn "termite -e \"$SHELL -c 'tmux new-session -A -s $USER'\"")
  , ("M4-S-<Return>"  , spawn $ myTerminal)

  -- screenshots
  , ("M4-p"           , spawn "ksnip -r")

  -- killing stuff
  , ("M4-c"           , kill)
  , ("M4-S-c"         , spawn "xkill")
  ]

-- Get the list of Firefox profiles dynamically
getFirefoxProfiles :: IO [String]
getFirefoxProfiles = do
    profilesIniPath <- expandPath "~/.mozilla/firefox/profiles.ini"
    fileExists <- doesFileExist profilesIniPath
    if not fileExists
        then error "profiles.ini not found!"
        else do
            profilesIni <- readFile profilesIniPath
            if null profilesIni
                then error "profiles.ini is empty!"
                else return $ parseProfiles profilesIni

-- Expand the tilde in a file path
expandPath :: String -> IO String
expandPath ('~':'/':rest) = do
    home <- getHomeDirectory
    return $ home ++ '/' : rest
expandPath path = return path

-- Parse profiles.ini to extract profile names
parseProfiles :: String -> [String]
parseProfiles iniContent =
    [ drop (length "Name=") line
    | line <- lines iniContent, "Name=" `isPrefixOf` line
    ]

-- Create the YAD command to show the profile chooser
yadCommand :: [String] -> String
yadCommand profiles =
    "yad --list --separator='' --title='firefox profile manager' --text='Select a Firefox profile' --column='Profiles' " ++ unwords (map escape profiles)
  where
    -- Escape profiles to ensure they are safely passed as arguments
    escape s = "'" ++ s ++ "'"

-- Function to launch Firefox with a profile or Tor Browser for "scratch"
launchFirefoxWithProfile :: X ()
launchFirefoxWithProfile = io $ do
    profiles <- getFirefoxProfiles
    -- Add the "scratch" profile to the list
    let allProfiles = "scratch" : profiles
    if null allProfiles
        then spawn "yad --error --text='No Firefox profiles found!'"
        else do
            let command =
                    "bash -c \""
                        ++ yadCommand allProfiles
                        ++ " | sed 's/|$//' | xargs -I {} bash -c 'if [ \"{}\" = \"scratch\" ]; then torbrowser-launcher; else firefox --no-remote -P \"{}\"; fi'\""
            -- Execute the command after showing it
            spawn $ command

-- Helper function to escape double quotes in the command
-- escapeDoubleQuotes :: String -> String
-- escapeDoubleQuotes = concatMap (\c -> if c == '"' then "\\\"" else [c])
