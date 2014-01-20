import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import System.IO
-- TBD:
--import XMonad.Layout.Tabbed

--import Data.Monoid
--import System.Exit
--import qualified XMonad.StackSet as W
--import qualified Data.Map as M

myManageHook = composeAll
    [ manageHook defaultConfig <+> manageDocks,
      isFullscreen --> doFullFloat,
      resource =? "FEZ.bin.x86" --> doFloat,
      resource =? "FEZ.bin.x86_64" --> doFloat,
      className =? "Gimp" --> doFloat,
      -- big picture mode?
      className =? "steam" --> doIgnore
    ]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/benley/.xmobarrc"
    xmonad $
      defaultConfig
        { borderWidth = 2,
          manageHook = myManageHook,
          handleEventHook = fullscreenEventHook,
          layoutHook = avoidStruts $ layoutHook defaultConfig,
          logHook = dynamicLogWithPP xmobarPP
              { ppOutput = hPutStrLn xmproc,
                ppTitle = xmobarColor "green" "" -- . shorten 70
              }
        }
        `additionalKeys`
            [ ((mod1Mask .|. mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock")
            ]
