import XMonad
import XMonad.Actions.Commands (defaultCommands, runCommand)
import XMonad.Actions.CopyWindow (copyWindow)
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageHelpers (doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Hooks.Place (placeHook, simpleSmart)
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Roledex
import XMonad.Layout.Tabbed (tabbed)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Themes (theme, donaldTheme)

import Data.Monoid (mconcat)
import qualified XMonad.StackSet as W
import qualified Data.Map

-- XMonad.Actions.CopyWindow.copyToAll as a ManageHook
-- derived from https://mail.haskell.org/pipermail/xmonad/2009-September/008643.html
doCopyToAll :: ManageHook
doCopyToAll = ask >>= doF . \w ws -> foldr (copyWindow w) ws (workspaces myConfig)

-- (currently unused, but...)
unfloat :: ManageHook
unfloat = ask >>= doF . W.sink

myManageHook = composeAll
  [ fullscreenManageHook
  , isFullscreen --> doFullFloat
  , isDialog     --> doCenterFloat
  , className =? "Gimp"           --> doFloat
  , resource  =? "Steam"          --> doFloat
  , className =? "plasmashell"    --> doFloat
  , className =? "pinentry"       --> doCenterFloat  -- matches for pinentry-qt
  , resource  =? "pinentry"       --> doCenterFloat  -- matches for pinentry-gtk (wtf?)
  , className =? "krunner"        --> doCenterFloat
  , title     =? "Slack Call Minipanel" --> (doFloat <+> doCopyToAll)
  -- I honestly don't know what the swapMaster part accomplishes here
  , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> (doCenterFloat <+> doF W.swapMaster)
  , placeHook simpleSmart
  ]

myLayoutHook = smartBorders $ desktopLayoutModifiers $
    myResizable
     ||| Mirror myResizable
     ||| fullscreenFull Full

    where myResizable = mouseResizableTile { masterFrac = 0.5
                                           , fracIncrement = 1/100
                                           , draggerType = FixedDragger 5 15
                                           }

{- More layouts:
 Grid
 ResizableTall 0 (3/100) (1/2) []
 Mirror (ResizableTall 0 (3/100) (1/2) [])
 Accordion
 floatSimpleDwmStyle
 ThreeColMid 1 (3/100) (2/3)
 tabbed shrinkText (theme donaldTheme)
 simpleDeco shrinkText defaultTheme Roledex
-}

-- Which key to use as the default modMask
-- mod1Mask: alt, mod4Mask: win
defaultModMask = mod1Mask

kde5Config = desktopConfig
    { terminal = "konsole"
    , keys = kde5Keys <+> keys desktopConfig
    }

kde5Keys conf@XConfig {XMonad.modMask = modm} = Data.Map.fromList
    [ ((modm,               xK_space), spawn "krunner")
    , ((modm .|. shiftMask, xK_q),
       spawn ("dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer "
              ++ "org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1"))
    , ((modm,               xK_p), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_p), setLayout (XMonad.layoutHook conf))
    ]

myKeyBindings XConfig {XMonad.modMask = modm} = Data.Map.fromList
    [ ((modm .|. controlMask, xK_y), defaultCommands >>= runCommand)
    , ((modm .|. shiftMask, xK_p), spawn "dmenu_run -i -p 'Launch:' -l 5 -fn 'Noto Sans:size=15'")
    , ((modm, xK_a), sendMessage ShrinkSlave)
    , ((modm, xK_z), sendMessage ExpandSlave)
    , ((modm, xK_g), goToSelected defaultGSConfig)
    , ((modm .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
    ]

myConfig =
    desktopConfig
    { modMask = defaultModMask
    , manageHook = manageHook kde5Config <+> myManageHook
    , layoutHook = myLayoutHook
    , borderWidth = 3
    , handleEventHook = mconcat
        [ XMonad.Hooks.EwmhDesktops.fullscreenEventHook
        , handleEventHook kde5Config
        ]
    , startupHook = startupHook kde5Config <+> spawnOnce "xcompmgr"
    , keys = myKeyBindings <+> keys desktopConfig -- <+> keys kde5Config
    , logHook = dynamicLogString myPP >>= xmonadPropLog
    , terminal = "konsole"
    }
    where myPP = xmobarPP { ppTitle = xmobarColor "#89DDFF" "#263238"
                          , ppCurrent = xmobarColor "#263238" "#89DDFF" . wrap "[" "]"
                          }

main = spawn "xmobar" >> xmonad myConfig
