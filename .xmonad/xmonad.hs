import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import System.Exit
import System.IO
-- TBD:
--import XMonad.Layout.Tabbed

--import Data.Monoid
--import qualified XMonad.StackSet as W
--import qualified Data.Map as M

myManageHook = composeAll
    [ manageHook gnomeConfig <+> manageDocks,
      isFullscreen --> doFullFloat,
      resource  =? "FEZ.bin.x86"    --> doFloat,
      resource  =? "FEZ.bin.x86_64" --> doFloat,
      resource  =? "tinyandbig"     --> doIgnore,
      className =? "Gimp"           --> doFloat,
      className =? "hl2_linux"      --> doFullFloat,
      className =? "steam"          --> doIgnore,  -- big picture mode?
      manageDocks
    ]

myLayoutHook = smartBorders (
                   avoidStruts (
                       Tall 1 (3/100) (1/2) ||| Mirror (Tall 1 (3/100) (1/2))
                       ) ||| noBorders (fullscreenFull Full)
                   )
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/benley/.xmobarrc"
    xmonad $ ewmh gnomeConfig {
                borderWidth = 2,
                manageHook = myManageHook,
                handleEventHook = XMonad.Hooks.EwmhDesktops.fullscreenEventHook,
                layoutHook = myLayoutHook,
                logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc,
                      ppTitle = xmobarColor "green" ""
                      -- add . shorten 70 above to limit title length in xmobar
                    }
        } `additionalKeys`
        [ ((mod1Mask .|. mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock"),
          ((mod1Mask .|. shiftMask, xK_q), io (exitWith ExitSuccess)),
          ((mod1Mask, xK_p), spawn "dmenu_run")
        ]
