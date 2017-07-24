import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
-- import XMonad.Config.Gnome
-- import XMonad.Config.Kde
-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
-- import XMonad.Hooks.FadeInactive
-- import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, doCenterFloat, isFullscreen, isDialog)
-- import XMonad.Hooks.UrgencyHook
-- import XMonad.Layout.Accordion
-- import XMonad.Layout.Circle
-- import XMonad.Layout.Grid
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.MouseResizableTile
-- import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Roledex
import XMonad.Layout.Tabbed (tabbed)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
-- import XMonad.Prompt
-- import XMonad.Prompt.Man
import XMonad.Util.EZConfig (additionalKeys)
-- import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Themes (theme, donaldTheme)
-- import System.Exit
-- import System.IO

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
  ] where unfloat :: ManageHook
          unfloat = ask >>= doF . W.sink

myLayoutHook = smartBorders $ desktopLayoutModifiers $
    myResizable
     ||| simpleDeco shrinkText defaultTheme Roledex
     ||| fullscreenFull Full
     ||| floatSimpleDwmStyle

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
-}

-- Which key to use as the default modMask
-- mod1Mask: alt, mod4Mask: win
-- modm = mod1Mask

kde5Config = desktopConfig
    { terminal = "konsole"
    , keys = kde5Keys <+> keys desktopConfig
    }

kde5Keys XConfig {modMask = modm} = Data.Map.fromList
    [ ((modm, xK_p), spawn "krunner")
    , ((modm .|. shiftMask, xK_q),
       spawn ("dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer "
              ++ "org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1"))
    ]

myKeyBindings =
    [ ((mod1Mask .|. controlMask, xK_y), commands >>= runCommand)
    , ((mod1Mask .|. shiftMask, xK_p),
       spawn "dmenu_run -i -p 'Launch:' -l 5 -fn 'Noto Sans:size=15'")
    , ((mod1Mask, xK_a), sendMessage ShrinkSlave)
    , ((mod1Mask, xK_z), sendMessage ExpandSlave)
    , ((mod1Mask, xK_g), goToSelected defaultGSConfig)
    --, ((mod1Mask, xK_F1), manPrompt defaultXPConfig)
    ]
    where commands = defaultCommands
          -- ^ List of commands to use with dmenu when you press ctrl-mod1-y

main =
    xmonad (kde5Config { modMask = mod1Mask
                       , manageHook = manageHook kde5Config <+> myManageHook
                       , layoutHook = myLayoutHook
                       , borderWidth = 3
                       , handleEventHook = mconcat
                           [ XMonad.Hooks.EwmhDesktops.fullscreenEventHook
                           , handleEventHook kde5Config
                           ]
                       , startupHook = spawnOnce "xcompmgr" <+> startupHook kde5Config
                       } `additionalKeys` myKeyBindings)
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
