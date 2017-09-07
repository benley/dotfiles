import qualified Data.Map
import Data.Monoid (mconcat)

import System.Taffybar.Hooks.PagerHints (pagerHints)

import XMonad
import XMonad.Actions.Commands (defaultCommands, runCommand)
import XMonad.Actions.CopyWindow (copyWindow)
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog, isInProperty)
import XMonad.Hooks.Place (placeHook, simpleSmart)
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ThreeColumns as ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (mkKeymap, removeKeysP)

-- XMonad.Actions.CopyWindow.copyToAll as a ManageHook
-- derived from https://mail.haskell.org/pipermail/xmonad/2009-September/008643.html
doCopyToAll :: ManageHook
doCopyToAll = ask >>= doF . \w ws -> foldr (copyWindow w) ws (workspaces myConfig)

-- (currently unused, but...)
unfloat :: ManageHook
unfloat = ask >>= doF . W.sink

myManageHook = composeAll
  [ fullscreenManageHook
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
  -- Don't manage splash windows (e.g. the ones krita and gimp show at startup)
  , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH" --> doIgnore
  , placeHook simpleSmart
  ]

myLayoutHook =
    smartBorders
    (ThreeColumns.ThreeColMid 1 (3/100) (1/2) ||| layoutHook desktopConfig)

---- I have no idea why, but mouseResizeableTile was causing notification
---- windows to disappear behind regular windows :-(
-- myLayoutHook =
--     smartborders $ desktopLayoutModifiers $
--     myResizable
--      ||| Mirror myResizable
--      ||| fullscreenFull Full
--     where myResizable = mouseResizableTile { masterFrac = 0.5
--                                            , fracIncrement = 1/100
--                                            , draggerType = FixedDragger 5 15
--                                            }

-- Which key to use as the default modMask
-- mod1Mask: alt, mod4Mask: win
defaultModMask = mod1Mask

myKeyBindings =
  flip mkKeymap
    [ ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight +5")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -5")
    , ("M-g", goToSelected defaultGSConfig)
    , ("C-M-l", spawn "xscreensaver-command -lock")
    , ("C-M-y", defaultCommands >>= runCommand)
    , ("S-M-p", spawn "dmenu_run -i -p 'Launch:' -l 5 -fn 'Noto Sans: size=12'")
    --, ("m_a", sendMessage ShrinkSlave)
    --, ("m_z", sendMessage ExpandSlave)
    ]

myConfig =
  desktopConfig
    { modMask = defaultModMask
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = myLayoutHook
    , borderWidth = 5
    , handleEventHook = mconcat
        [ XMonad.Hooks.EwmhDesktops.fullscreenEventHook
        , handleEventHook desktopConfig
        ]
    , keys = myKeyBindings <+> keys desktopConfig
    , terminal = "konsole"
    , normalBorderColor = "#263238"
    , focusedBorderColor = "#ea9560"
    } `removeKeysP` ["M-b"]

main = xmonad $ pagerHints myConfig
