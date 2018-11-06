{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (when)
import qualified Data.Map
import Data.Monoid (mconcat)
import System.Exit (exitSuccess)
import System.Posix.User

import System.Taffybar.Support.PagerHints (pagerHints)

import XMonad
import XMonad.Actions.Commands (defaultCommands, runCommand)
import XMonad.Actions.CopyWindow (copyWindow)
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog, isInProperty, doFullFloat)
import XMonad.Hooks.Place (placeHook, simpleSmart)
import XMonad.Layout.BoringWindows
import XMonad.Layout.Decoration
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenManageHook)
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Simplest
import XMonad.Layout.ThreeColumns as ThreeColumns
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Cursor as C
import XMonad.Util.EZConfig (checkKeymap, mkKeymap, removeKeysP)
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import XMonad.Util.Themes
import qualified XMonad.Util.Dmenu as Dmenu
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)

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
  , className =? "Pavucontrol"    --> doCenterFloat
  , className =? "plasmashell"    --> doFloat
  , title     =? "Bluetooth Devices" --> doFloat
  , className =? "pinentry"       --> doCenterFloat  -- matches for pinentry-qt
  , resource  =? "pinentry"       --> doCenterFloat  -- matches for pinentry-gtk (wtf?)
  , className =? "krunner"        --> doCenterFloat
  , className =? "Nm-connection-editor" --> doFloat
  , className =? "Kupfer.py"      --> doCenterFloat
  , title     =? "PlayOnLinux"    --> doFloat
  -- , className =? "BridgeConstructor.x86" --> doFullFloat
  , title     =? "Slack Call Minipanel" --> (doFloat <+> doCopyToAll)
  , title     =? "Steam Keyboard" --> doIgnore
  -- I honestly don't know what the swapMaster part accomplishes here
  , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> (doCenterFloat <+> doF W.swapMaster)
  -- Don't manage splash windows (e.g. the ones krita and gimp show at startup)
  , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH" --> doIgnore
  -- Don't manage notification overlays
  , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION" --> doIgnore
  , placeHook simpleSmart
  ]

-- Dynamic property change hook: catches Chrome apps, whose titles are not set until
-- after the window is created
myDynHook = composeAll
  [ title =? "Google Hangouts - benley@gmail.com" --> doF (W.shift "7")
  , title =? "Signal" <&&> className =? "Google-chrome" --> doF (W.shift "7")
  ]

subTabbedWithTheme :: (Eq a, LayoutModifier (Sublayout Simplest) a, LayoutClass l a)
                   => Theme
                   -> l a
                   -> ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) (ModifiedLayout (Sublayout Simplest) l) a
subTabbedWithTheme t x = addTabs shrinkText t $ subLayout [] Simplest x

myLayoutHook =
    windowNavigation $
    subTabbedWithTheme tabTheme $ -- (theme kavonFireTheme) $
    boringWindows $
    desktopLayoutModifiers $
    smartBorders $

    Tall 1 (3/100) (1/2)

    where tabTheme = def { decoHeight = 30
                         , activeColor = "#ea9560"
                         , activeTextColor = "#31363b"
                         , fontName = "xft: Noto Sans-10"
                         }

-- Which key to use as the default modMask
-- mod1Mask: alt, mod4Mask: win
defaultModMask = mod4Mask

defaultFont = "PragmataPro"

getCurrentBrightness = do
  cur <- runProcessWithInput "light" ["-b", "-G"] ""
  return $ read cur

doBrightnessUp :: X ()
doBrightnessUp = do
  cur <- getCurrentBrightness
  io $ if cur < 0.19
    then safeSpawn "light" ["-b", "-S", "0.19"]
    else safeSpawn "light" ["-b", "-A", "5"]

doBrightnessDown :: X ()
doBrightnessDown = do
  cur <- getCurrentBrightness
  io $ if cur <= 6 && cur > 0.2
    then safeSpawn "light" ["-b", "-S", "0.11"]
    else safeSpawn "light" ["-b", "-U", "5"]

doXrandrThing :: X ()
doXrandrThing =
  safeSpawn "xrandr" ["--auto"] >>
  safeSpawn "xrandr" ["--output", "eDP1",
                      "--mode", "2560x1440",
                      "--pos", "3840x720",
                      "--rotate", "normal",
                      "--output", "DP1",
                      "--primary",
                      "--mode", "3840x2160",
                      "--pos", "0x0",
                      "--rotate", "normal"]

myKeys =
  -- keymap reference: http://hackage.haskell.org/package/xmonad-contrib-0.14/docs/XMonad-Util-EZConfig.html#v:mkKeymap
    [ ("<XF86AudioLowerVolume>", safeSpawn "amixer" ["--quiet", "set", "Master", "5%-", "unmute"])
    , ("<XF86AudioRaiseVolume>", safeSpawn "amixer" ["--quiet", "set", "Master", "5%+", "unmute"])
    , ("<XF86AudioMute>",        safeSpawn "amixer" ["--quiet", "set", "Master", "toggle"])
    , ("<XF86AudioMicMute>",     safeSpawn "amixer" ["--quiet", "set", "Capture", "toggle"])
    , ("<XF86MonBrightnessUp>",   doBrightnessUp)
    , ("<XF86MonBrightnessDown>", doBrightnessDown)
    , ("<XF86PowerOff>", quitWithWarning)
    , ("<XF86Display>", doXrandrThing)
    , ("M-g", goToSelected defaultGSConfig)
    , ("M-M1-S-l", safeSpawn "xset" ["s", "activate"])
    , ("C-M-y", commands >>= runCommand)
    , ("S-M-p", safeSpawn "dmenu_run" (["-p", "cmdline:"] ++ dmenu_args))
    , ("S-M-n", spawnNetworkMenu)
    , ("M-p", safeSpawn "j4-dmenu-desktop" ["--dmenu=dmenu -p app: " ++ unwords dmenu_args])
    , ("M-S-q", quitWithWarning)
    , ("M-a", sendMessage ShrinkSlave)
    , ("M-z", sendMessage ExpandSlave)
    , ("C-M-h", sendMessage $ pullGroup L)
    , ("C-M-j", sendMessage $ pullGroup D)
    , ("C-M-k", sendMessage $ pullGroup U)
    , ("C-M-l", sendMessage $ pullGroup R)
    , ("C-M-m", withFocused (sendMessage . MergeAll))
    , ("C-M-u", withFocused (sendMessage . UnMerge))
    , ("M-,",   onGroup W.focusUp')
    , ("M-.",   onGroup W.focusDown')
    -- , ("M-S-`", onGroup W.focusUp')
    -- , ("M-`",   onGroup W.focusDown')
    , ("M-S-,", windows W.swapUp)
    , ("M-S-.", windows W.swapDown)
    , ("M-M1-h", sendMessage Shrink)
    , ("M-M1-l", sendMessage Expand)
    , ("M-h", sendMessage $ Go L)
    , ("M-j", sendMessage $ Go D)
    , ("M-k", sendMessage $ Go U)
    , ("M-l", sendMessage $ Go R)
    , ("M-S-h", sendMessage $ Swap L)
    , ("M-S-j", sendMessage $ Swap D)
    , ("M-S-k", sendMessage $ Swap U)
    , ("M-S-l", sendMessage $ Swap R)
    , ("M-=", sendMessage $ IncMasterN 1)
    , ("M--", sendMessage $ IncMasterN (-1))
    , ("M-<Tab>", focusDown)
    , ("M-S-<Tab>", focusUp)
    ] where dmenu_args = ["-i", "-l", "10", "-fn", defaultFont]
            commands :: X [(String, X ())]
            commands = do
              dc <- defaultCommands
              return (dc ++ [("nm-menu", spawnNetworkMenu)])

spawnNetworkMenu :: X ()
spawnNetworkMenu = safeSpawn "networkmanager_dmenu" ["-i", "-fn", defaultFont]

quitWithWarning :: X ()
quitWithWarning = do
    s <- Dmenu.menuArgs "dmenu" [ "-p", "Quit?", "-nb", "red", "-nf", "black",
                                  "-sf", "white", "-sb", "black",
                                  "-i", "-l", "5", "-fn", defaultFont]
                                ["Nope", "Logout", "Suspend", "Reboot", "Power Off"]
    when (s == "Logout") (io exitSuccess)
    when (s == "Suspend") (safeSpawn "systemctl" ["suspend"])
    when (s == "Reboot") (safeSpawn "systemctl" ["reboot"])
    when (s == "Power Off") (safeSpawn "systemctl" ["poweroff"])

myStartupHook =
    C.setDefaultCursor C.xC_left_ptr

myConfig =
  desktopConfig
    { modMask = defaultModMask
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = myLayoutHook
    , borderWidth = 5
    , handleEventHook = mconcat
        [ XMonad.Hooks.EwmhDesktops.fullscreenEventHook
        , handleEventHook desktopConfig
        , dynamicPropertyChange "WM_NAME" myDynHook
        ]
    , startupHook = do
        return ()  -- (see checkKeymap docs for why this is here)
        checkKeymap myConfig myKeys
        (myStartupHook <+> startupHook desktopConfig)
    , keys = flip mkKeymap myKeys <+> keys desktopConfig
    , normalBorderColor = "#263238"
    , focusedBorderColor = "#ea9560"
    } `removeKeysP` ["M-b"]

main = do
  -- shell <- getLoginName >>= getUserEntryForName >>= return . userShell

  xmonad $ pagerHints $ myConfig {
    -- terminal = "emacsclient -n -c -a '' -e '(ansi-term \"" ++ shell ++ "\")'"
    terminal = "konsole"
    }
