{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Config.Plasma.Util where

import XMonad
import XMonad.Hooks.ManageHelpers

kdeNoBorders = isKdeOverride <||> isPopupMenu <||> isKdeOSD

isDock         = isInType     "_NET_WM_WINDOW_TYPE_DOCK"
isDesktop      = isInType     "_NET_WM_WINDOW_TYPE_DESKTOP"
isNotification = isInType     "_NET_WM_WINDOW_TYPE_NOTIFICATION"
isPopupMenu    = isInType     "_NET_WM_WINDOW_TYPE_POPUP_MENU"
isKdeOSD       = isInType "_KDE_NET_WM_WINDOW_TYPE_ON_SCREEN_DISPLAY"
isKdeOverride  = isInType "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"

isInType :: String -> Query Bool
isInType = isInProperty "_NET_WM_WINDOW_TYPE"
