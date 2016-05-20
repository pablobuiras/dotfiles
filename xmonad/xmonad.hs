
import Control.Monad

import XMonad
import XMonad.Config.Gnome
import XMonad.Util.Replace
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Circle
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.SpawnOn
import qualified XMonad.Util.Dzen as DZ
import qualified XMonad.StackSet as W

myWorkspaces  = clickable  $
                ["^i(" ++ myBarIconDir ++ "web.xbm) web"
                ,"^i(" ++ myBarIconDir ++ "docs.xbm) tex"
                ,"^i(" ++ myBarIconDir ++ "shell.xbm) term"
                ,"^i(" ++ myBarIconDir ++ "tunes.xbm) media"]
--                ,"^i(" ++ myBarIconDir ++ "mail.xbm) mail"]
      where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                              (i,ws) <- zip [1..] l,
                              let n = i ]  


baseConfig = defaultConfig


myDmenu  = "dmenu_run -fn " ++ myFont ++ " -h 30 -nb '#000000' -nf '#b2b2b2' -sb '#ffee55' -sf '#000000' -p 'Run > '"


myManageHook = composeAll (
    [ manageHook baseConfig
    , role =? "browser" --> viewShift (myWorkspaces!!0)
    , className =? "Emacs" --> viewShift (myWorkspaces!!1)
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "stalonetray" --> doIgnore
    , className =? "trayer" --> doIgnore
    , className =? "Unity-2d-launcher" --> doFloat
    , isSplash --> doIgnore
    , className =? "Gtkdialog"      --> doCenterFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "dmenu"          --> doFloat
    , maybeToDefinite (isFullscreen -?> doFullFloat)
    ])
  where viewShift = doF . liftM2 (.) W.greedyView W.shift
        role = stringProperty "WM_WINDOW_ROLE"

isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor active background . pad 

    , ppVisible         = dzenColor inactive background . wrap "<" ">"
                          
    -- display other workspaces which contain windows as a brighter grey
    , ppHidden          = dzenColor inactive background . pad 

    -- display other workspaces with no windows as a normal grey
    , ppHiddenNoWindows = dzenColor nowindow background . pad 

    -- display the current layout
    , ppLayout          = dzenColor inactive background . (\x ->
            case x of
              "Full"                          ->
                "^i(" ++ myWMIconDir ++ "wm_full.xbm)"
              "Spacing 5 ResizableTall"       ->
                "^i(" ++ myWMIconDir ++ "wm_tall5.xbm)"
              "ResizableTall"                 ->
                "^i(" ++ myWMIconDir ++ "wm_tall.xbm)"
              "SimplestFloat"                 ->
                "^i(" ++ myWMIconDir ++ "wm_float.xbm)"
              "Circle"                        ->
                "^i(" ++ myWMIconDir ++ "wm_circle.xbm)"
              _                               ->
                "^i(" ++ myWMIconDir ++ "wm_default.xbm)"
                                                          )

    -- if a window on a hidden workspace needs my attention, color it so
    , ppUrgent          = dzenColor "#ff0000" background . pad

    -- shorten if it goes over 100 characters
    , ppTitle           = shorten 100  

    -- no separator between workspaces
    , ppWsSep           = ""

    -- put a few spaces between each object
    , ppSep             = "  "

    -- output to the handle we were given as an argument
    , ppOutput          = hPutStrLn h
    }


myLayout = smartBorders $ avoidStruts $ onWorkspace texWS texLayout $ btLayout
  where texWS = myWorkspaces !! 1
        texLayout = btLayout
        btLayout = layoutHook baseConfig

myConfig =  baseConfig { workspaces = myWorkspaces
		       , terminal   = "sudo -u pablo urxvt"
		       , focusedBorderColor = "#af1f1f"
                       , manageHook = myManageHook
                       , layoutHook = myLayout
                       , handleEventHook = fullscreenEventHook
                       , modMask = mod4Mask
                       , borderWidth = 3
                       , startupHook = myStartupHook              
                       , focusFollowsMouse = False }
            `removeKeysP` ["M-<Space>", "M-S-<Space>", "M-p"]
            `additionalKeysP` [ ("M-p", spawn myDmenu)
	    		      , ("C-M-l", spawn "slimlock")
                              , ("M-<Tab>", sendMessage NextLayout)
                              , ("M-S-<Tab>", sendMessage FirstLayout)
                              , ("M-q", spawn "killall volumeicon conky dzen2 trayer; xmonad --recompile; xmonad --restart")]

myFont = "'Enriqueta':size=12:antialias=true:hinting=true:style=Bold"

myWMIconDir = "/home/pablo/.xmonad/dzen2/wm/"
myBarIconDir = "/home/pablo/.xmonad/dzen2/workspaces/"

commonDzen = "-fg '" ++ foreground ++ "' -bg '" ++ background ++
             "' -fn " ++ myFont

dzen2rel = "/home/pablo/.xmonad/dzen2rel"

myStartupHook = do spawnOn (myWorkspaces!!2) "sudo -u pablo urxvt"
                   io myIO
  where myIO =
          do let dzenStr = dzen2rel ++ " R 380 -p -xs 1 " ++ commonDzen
                           ++ " -ta r -e 'onstart=lower'"
             spawn $ "conky -qc ~/.xmonad/data/conky/dzen | " ++ dzenStr
             spawn myTray
             forM_ startupApps spawn
             return ()

myTray = "trayer --margin 380 --edge top --align right --SetDockType true --SetPartialStrut true  --expand true --widthtype request --transparent true --alpha 0 --tint '#000000' --height 22 --distance 4"

main = do replace
--          spawn "autorandr --change"
          let dzenStr = dzen2rel ++ " L 380 -p -xs 1 " ++ commonDzen
                        ++ " -ta l -e 'onstart=lower'"
          d <- spawnPipe dzenStr
          xmonad $ withUrgencyHook myUrgencyHook (myConfig { logHook = 
myLogHook d })

myUrgencyHook = dzenUrgencyHook
  { args = ["-xs", "1",
            "-bg", background,
            "-fg", "green",
            "-fn", myFont] }

--
-- Colours
--
background      = "#000000"
foreground      = "#ffffff"
active          = "#ffee55"
inactive        = "#bbbbbb"
nowindow        = "#404040"
nborder         = "#343638"
aborder         = "#111111"

--
-- Wallpaper
--
wallpaperPath = "/home/pablo/Pictures/wallpaper.jpg"

--
-- Startup apps
--

startupApps = [ "sleep 1"
              , "feh --bg-fill " ++ wallpaperPath
              , "volumeicon"
              , "qxkb" ]
