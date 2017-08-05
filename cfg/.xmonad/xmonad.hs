import XMonad
import XMonad.Actions.Commands (defaultCommands, runCommand)
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
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
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Themes (theme, donaldTheme)

import Data.Monoid (mconcat)
import qualified XMonad.StackSet as W
import qualified Data.Map

myManageHook = composeAll
  [ fullscreenManageHook
  , isFullscreen --> doFullFloat
  , isDialog     --> doCenterFloat
  , resource  =? "FEZ.bin.x86"    --> doFloat
  , resource  =? "FEZ.bin.x86_64" --> doFloat
  , resource  =? "tinyandbig"     --> doIgnore
  , className =? "Gimp"           --> doFloat
  , className =? "hl2_linux"      --> doFullFloat
  , className =? "steam"          --> doIgnore  -- big picture mode?
  , className =? "Steam"          --> doFloat
  , className =? "plasmashell"    --> doFloat
  , className =? "pinentry"       --> doCenterFloat
  , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> doCenterFloat
  , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> doF W.swapMaster
  , placeHook simpleSmart
  ] where unfloat :: ManageHook
          unfloat = ask >>= doF . W.sink

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

kde5Keys conf@(XConfig {XMonad.modMask = modm}) = Data.Map.fromList
    [ ((modm,               xK_space), spawn "krunner")
    , ((modm .|. shiftMask, xK_q),
       spawn ("dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer "
              ++ "org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1"))
    , ((modm,               xK_p), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_p), setLayout (XMonad.layoutHook conf))
    ]

myKeyBindings XConfig {XMonad.modMask = modm} = Data.Map.fromList
    [ ((modm .|. controlMask, xK_y), defaultCommands >>= runCommand)
    , ((modm .|. shiftMask, xK_space), spawn "dmenu_run -i -p 'Launch:' -l 5 -fn 'Noto Sans:size=15'")
    , ((modm, xK_a), sendMessage ShrinkSlave)
    , ((modm, xK_z), sendMessage ExpandSlave)
    , ((modm, xK_g), goToSelected defaultGSConfig)
    ]

main =
    xmonad (kde5Config { modMask = defaultModMask
                       , manageHook = manageHook kde5Config <+> myManageHook
                       , layoutHook = myLayoutHook
                       , borderWidth = 3
                       , handleEventHook = mconcat
                           [ XMonad.Hooks.EwmhDesktops.fullscreenEventHook
                           , handleEventHook kde5Config
                           ]
                       , startupHook = spawnOnce "xcompmgr" <+> startupHook kde5Config
                       , keys = keys kde5Config <+> myKeyBindings
                       })
{-
  --If not using the fancy gnome session with its own status bar, uncomment:
  --do xmproc <- spawnPipe "/usr/bin/xmobar /home/benley/.xmobarrc"
  xmonad $ withUrgencyHookC
             dzenUrgencyHook { args = ["-bg", "darkgreen",
                                       "-fg", "white",
                                       "-xs", "1",
                                       "-fn", "Noto Sans:size=20"] }
             urgencyConfig { suppressWhen = Focused, remindWhen = Every 60 }
         $ withUrgencyHookC
             BorderUrgencyHook { urgencyBorderColor = "#00ff00" }
             urgencyConfig { suppressWhen = XMonad.Hooks.UrgencyHook.Never }
         $ gnomeConfig
             { terminal = "xterm"
             , borderWidth = 3
             , focusedBorderColor = "#ff0000"
             , normalBorderColor = "#dddddd"
             , manageHook = myManageHook
             , handleEventHook = mconcat
                 [ XMonad.Hooks.EwmhDesktops.fullscreenEventHook
                 , handleEventHook gnomeConfig
                 ]
             , layoutHook = myLayoutHook
             --logHook = do
             --  dynamicLogWithPP xmobarPP
             --    { ppOutput = hPutStrLn xmproc
             --    , ppTitle = xmobarColor "green" ""
             --      -- add . shorten 70 above to limit title length in xmobar
             --    }
             , logHook = logHook gnomeConfig <+> fadeInactiveLogHook 0.9
             , startupHook = do
                 startupHook gnomeConfig
                 spawnOnce "xcompmgr -fF"
             , modMask = modm
             } `additionalKeys` myKeyBindings

-}
