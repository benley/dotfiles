import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import System.Exit
import System.IO

--import Data.Monoid
--import qualified XMonad.StackSet as W
--import qualified Data.Map as M

myManageHook = composeAll
    [ manageHook gnomeConfig,
      fullscreenManageHook,
      isFullscreen --> doFullFloat,
      isDialog     --> doCenterFloat,
      resource  =? "FEZ.bin.x86"    --> doFloat,
      resource  =? "FEZ.bin.x86_64" --> doFloat,
      resource  =? "tinyandbig"     --> doIgnore,
      className =? "Gimp"           --> doFloat,
      className =? "hl2_linux"      --> doFullFloat,
      className =? "steam"          --> doIgnore,  -- big picture mode?
      className =? "Steam"          --> doFloat,
      manageDocks
    ]

myLayoutHook = smartBorders $ avoidStruts (
       Tall 1 (3/100) (1/2) ||| Mirror (Tall 1 (3/100) (1/2)) ||| simpleTabbed
       ) ||| noBorders (fullscreenFull Full)

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/benley/.xmobarrc"
    xmonad $ ewmh gnomeConfig {
                borderWidth = 2,
                manageHook = myManageHook,
                handleEventHook = XMonad.Hooks.EwmhDesktops.fullscreenEventHook,
                layoutHook = myLayoutHook,
                logHook = do
                  dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc,
                      ppTitle = xmobarColor "green" ""
                      -- add . shorten 70 above to limit title length in xmobar
                    }
                  logHook desktopConfig
        } `additionalKeys`
        [ ((mod1Mask .|. mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock"),
          ((mod1Mask .|. shiftMask, xK_q), io (exitWith ExitSuccess)),
          ((mod1Mask, xK_p), spawn "dmenu_run")
        ]
