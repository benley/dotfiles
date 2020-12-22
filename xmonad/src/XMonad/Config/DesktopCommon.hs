{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Config.DesktopCommon where

import           XMonad
import           XMonad.StackSet as W

import           XMonad.Config.Desktop
import           XMonad.Config.Plasma.Util
import           XMonad.Config.Plasma.Layers

import           XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.StateHook

import           XMonad.Layout.BorderConfiguration
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.Minimize
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Fullscreen.NoBorders

import           XMonad.Util.NamedScratchpad

desktop conf = withLayers . withStateHandle . ewmhNSP $ conf
    { layoutHook = borderLayout .
                   minimize . boringWindows .
                   fullscreenFloat . fullscreenFocus .
                   avoidStruts .
                   layoutHook $ conf
    , handleEventHook = composeAll [ setBorderEventHook
                                   , minimizeEventHook
                                   , fullscreenBorderEventHook
                                   , fullscreenEventHook
                                   , handleEventHook conf
                                   , docksEventHook ]
    , manageHook = composeAll [ manageDocks
                              , isNotification         --> doIgnore
                              , fullscreenManageHook
                              , manageHook conf ]
    , startupHook = startupHook desktopConfig >> docksStartupHook >> startupHook conf }

ewmhNSP :: XConfig a -> XConfig a
ewmhNSP c = c { startupHook     = ewmhDesktopsStartup <+> startupHook c
              , handleEventHook = ewmhDesktopsEventHook <+> handleEventHook c
              , logHook         = ewmhDesktopsLogHookCustom namedScratchpadFilterOutWorkspace <+> logHook c }
