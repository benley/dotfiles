{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (when)
import qualified Data.Map
import Data.Monoid (mconcat)
import System.Exit (exitSuccess)

import XMonad
import XMonad.Actions.CopyWindow (copyWindow)
import qualified XMonad.Actions.CycleWS as CycleWS
-- TODO: consider XMonad.Actions.Warp instead of UpdatePointer
import XMonad.Actions.UpdatePointer (updatePointer)
import qualified XMonad.Actions.WindowBringer as WB
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
import XMonad.Config.Plasma (kde5Config)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog, isInProperty, doFullFloat)
import XMonad.Hooks.Place (placeHook, simpleSmart)
import XMonad.Layout.BoringWindows
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenManageHook)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import qualified XMonad.Prompt as P
import qualified XMonad.Prompt.Window as PW
import qualified XMonad.StackSet as W
-- import qualified XMonad.Util.Cursor as C
import XMonad.Util.EZConfig (checkKeymap, mkKeymap, removeKeysP)
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import qualified XMonad.Util.Dmenu as Dmenu
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)

-- | XMonad.Actions.CopyWindow.copyToAll as a ManageHook
-- derived from https://mail.haskell.org/pipermail/xmonad/2009-September/008643.html
doCopyToAll :: ManageHook
doCopyToAll = ask >>= doF . \w ws -> foldr (copyWindow w) ws (workspaces myConfig)

-- | The opposite of doFloat (currently unused)
unfloat :: ManageHook
unfloat = ask >>= doF . W.sink

myManageHook = composeAll
  [ fullscreenManageHook
  -- , isDialog  --> doCenterFloat
  -- , className =? "Gimp"           --> doFloat
  , className =? "Pavucontrol"    --> doCenterFloat
  , title     =? "Bluetooth Devices" --> doFloat
  , className =? "pinentry"       --> doCenterFloat  -- matches for pinentry-qt
  , resource  =? "pinentry"       --> doCenterFloat  -- matches for pinentry-gtk (wtf?)
  , className =? "Gcr-prompter"   --> doCenterFloat  -- yet another pinentry variant
  -- , className =? "krunner"        --> doIgnore >> doCenterFloat
  , className =? "Nm-connection-editor" --> doFloat
  , className =? "Kupfer.py"      --> doCenterFloat
  , title     =? "PlayOnLinux"    --> doFloat
  , title     =? "Slack Call Minipanel" --> (doFloat <+> doCopyToAll)
  , title     =? "Steam Keyboard" --> doIgnore
  , title     =? "Picture in picture" --> doFloat    -- youtube PiP
  -- I have no idea what this next line accomplishes or why I added it
  -- , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> (doCenterFloat <+> doF W.swapMaster)
  -- , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH" --> doIgnore
  -- , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION" --> doIgnore
  -- found this at https://bitbucket.org/f1u77y/xmonad-config/src/aa82413576cc8d6713d26ed7682e542f19752800/lib/XMonad/Config/Plasma/Layers.hs
  -- , isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_ON_SCREEN_DISPLAY" --> doIgnore

  -- Facebook Messenger
  , appName =? "crx_hlmjgdhfhcgjmdnimnjlomhklloejdbi" --> doF (W.shift "7")
  -- Google Hangouts
  , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doF (W.shift "7")
  , placeHook simpleSmart
  ]

-- | Dynamic property change hook: useful for Chrome apps, whose titles are
-- | not set until after the window is created
myDynHook = composeAll
  [
    -- title =? "Google Hangouts - benley@gmail.com" --> doF (W.shift "7"),
    -- title =? "Signal" <&&> className =? "Google-chrome" --> doF (W.shift "7"),
  ]

myLayoutHook =
    -- boringWindows $
    desktopLayoutModifiers $ smartBorders $
    (Tall 1 (3/100) (1/2) |||
     ThreeCol 1 (3/100) (1/2) |||
     ThreeColMid 1 (3/100) (1/2) |||
     (tabbed shrinkText def)
    )

defaultFont = "PragmataPro"

-- currently unused
getCurrentBrightness = do
  cur <- runProcessWithInput "light" ["-b", "-G"] ""
  return $ read cur

-- currently unused
doBrightnessUp :: X ()
doBrightnessUp = do
  cur <- getCurrentBrightness
  io $ if cur < 0.19
    then safeSpawn "light" ["-b", "-S", "0.19"]
    else safeSpawn "light" ["-b", "-A", "5"]

-- currently unused
doBrightnessDown :: X ()
doBrightnessDown = do
  cur <- getCurrentBrightness
  io $ if cur <= 6 && cur > 0.2
    then safeSpawn "light" ["-b", "-S", "0.11"]
    else safeSpawn "light" ["-b", "-U", "5"]

dmenuArgs = ["-i", "-l", "10", "-fn", defaultFont]

wbConfig = def { WB.menuArgs = dmenuArgs }

-- | currently unused: Keys for volume, brightness, etc.
-- | Useful when not running a desktop manager.
-- | keymap reference: http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Util-EZConfig.html#v:mkKeymap
mediaKeys =
    [ ("<XF86AudioLowerVolume>", safeSpawn "amixer" ["--quiet", "set", "Master", "5%-", "unmute"])
    , ("<XF86AudioRaiseVolume>", safeSpawn "amixer" ["--quiet", "set", "Master", "5%+", "unmute"])
    , ("<XF86AudioMute>",        safeSpawn "amixer" ["--quiet", "set", "Master", "toggle"])
    , ("<XF86AudioMicMute>",     safeSpawn "amixer" ["--quiet", "set", "Capture", "toggle"])
    , ("<XF86MonBrightnessUp>",   doBrightnessUp)
    , ("<XF86MonBrightnessDown>", doBrightnessDown)
    , ("<XF86PowerOff>", quitWithWarning)
    -- , ("<XF86Display>", doXrandrThing)
    ]

myKeys =
    [ ("M-g", WB.gotoMenuConfig wbConfig)
    , ("M-S-g", WB.bringMenuConfig wbConfig)
    , ("M-p", safeSpawn "dmenu_run" (["-p", "cmdline:"] ++ dmenuArgs))
    , ("M-S-p", safeSpawn "j4-dmenu-desktop" ["--dmenu=dmenu -p app: " ++ unwords dmenuArgs])
    , ("M-S-q", spawn "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1")
    , ("M-[", CycleWS.prevWS)
    , ("M-]", CycleWS.nextWS)
    , ("M-S-[", CycleWS.shiftToPrev >> CycleWS.prevWS)
    , ("M-S-]", CycleWS.shiftToNext >> CycleWS.nextWS)
    ]

-- currently unused because I'm running KDE, but if I need it again...
quitWithWarning :: X ()
quitWithWarning = do
  -- TODO: use dmenuMap here, probably?
    s <- Dmenu.menuArgs "dmenu" [ "-p", "Quit?", "-nb", "red", "-nf", "black",
                                  "-sf", "white", "-sb", "black",
                                  "-i", "-l", "5", "-fn", defaultFont]
                                ["Nope", "Logout", "Suspend", "Reboot", "Power Off"]
    when (s == "Logout") (io exitSuccess)
    when (s == "Suspend") (safeSpawn "systemctl" ["suspend"])
    when (s == "Reboot") (safeSpawn "systemctl" ["reboot"])
    when (s == "Power Off") (safeSpawn "systemctl" ["poweroff"])

myConfig =
  desktopConfig
    { modMask = mod4Mask  -- mod4 == windows key, mod1 == alt
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = myLayoutHook
    , borderWidth = 2
    , handleEventHook = mconcat
        [ XMonad.Hooks.EwmhDesktops.fullscreenEventHook
        , handleEventHook desktopConfig
        , dynamicPropertyChange "WM_NAME" myDynHook
        ]
    , logHook = logHook desktopConfig >> updatePointer (0.5, 0.5) (0, 0)
    , startupHook = do
        return ()  -- (see checkKeymap docs for why this is here)
        checkKeymap myConfig myKeys
        -- C.setDefaultCursor C.xC_left_ptr
        startupHook desktopConfig
    , keys = flip mkKeymap myKeys <+> keys desktopConfig
    -- , normalBorderColor = "#263238"
    -- , focusedBorderColor = "#ea9560"
    , terminal = "konsole"
    }

main = launch (kde5Config myConfig)
-- main = launch myConfig
