import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
-- import XMonad.Layout.Accordion
-- import XMonad.Layout.Circle
-- import XMonad.Layout.Grid
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.MouseResizableTile
-- import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Roledex
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
-- import XMonad.Prompt
-- import XMonad.Prompt.Man
import XMonad.Util.EZConfig --(additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
import System.Exit
import System.IO

import Data.Monoid (mconcat)
--import qualified XMonad.StackSet as W
--import qualified Data.Map as M

myManageHook = composeAll
  [ manageHook gnomeConfig
  , fullscreenManageHook
  , isFullscreen --> doFullFloat
  , isDialog     --> doCenterFloat
  , resource  =? "FEZ.bin.x86"    --> doFloat
  , resource  =? "FEZ.bin.x86_64" --> doFloat
  , resource  =? "tinyandbig"     --> doIgnore
  , className =? "Gimp"           --> doFloat
  , className =? "hl2_linux"      --> doFullFloat
  , className =? "steam"          --> doIgnore  -- big picture mode?
  , className =? "Steam"          --> doFloat
  , manageDocks
  ]

myLayoutHook = desktopLayoutModifiers (
  mouseResizableTile
    { masterFrac = 0.5
    , fracIncrement = (1/100)
    , draggerType = FixedDragger 5 15 }
  ||| simpleDeco shrinkText defaultTheme Roledex
  ||| noBorders (tabbed shrinkText (theme donaldTheme))
  ||| ThreeColMid 1 (3/100) (2/3)
  ) ||| noBorders (fullscreenFull Full)

{- More layouts:
 Grid
 ResizableTall 1 (3/100) (1/2) []
 Mirror (ResizableTall 1 (3/100) (1/2) [])
 Accordion
 floatSimpleDwmStyle
-}

-- Which key to use as the default modMask
-- mod1Mask: alt, mod4Mask: win
modm = mod1Mask

myKeyBindings =
  [ ((mod1Mask .|. mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock")
  , ((modm .|. shiftMask, xK_q), spawn "gnome-session-quit")
  , ((modm .|. controlMask, xK_y), commands >>= runCommand)
  , ((modm .|. shiftMask, xK_p), gnomeRun)
  , ((modm, xK_p), spawn "dmenu_run -i -p 'Launch:' -l 5 -fn 'Noto Sans:size=15'")
  , ((modm, xK_a), sendMessage ShrinkSlave)
  , ((modm, xK_z), sendMessage ExpandSlave)
  , ((modm, xK_g), goToSelected defaultGSConfig)
  -- , ((mod1Mask, xK_F1), manPrompt defaultXPConfig)
  ]

-- List of commands to use with dmenu when you press ctrl-mod1-y:
commands = defaultCommands

main = do
  --If not using the fancy gnome session with its own status bar, uncomment:
  --xmproc <- spawnPipe "/usr/bin/xmobar /home/benley/.xmobarrc"
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
             { terminal = "terminator"
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
