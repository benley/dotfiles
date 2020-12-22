module XMonad.Layout.Fullscreen.NoBorders where

import Control.Monad
import Data.Monoid
import Data.Maybe

import XMonad
import XMonad.Layout.BorderConfiguration
import XMonad.Util.WindowProperties
import XMonad.Util.XUtils

fullscreenBorderEventHook :: Event -> X All
fullscreenBorderEventHook (ClientMessageEvent { ev_window = win
                                              , ev_message_type = a
                                              , ev_data = (action:state1:state2:_)}) = do
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] `fmap` getProp32 wmstate win
  let isFull = fi fullsc `elem` wstate
      remove = 0
      add = 1
      toggle = 2
  when (a == wmstate && fi fullsc `elem` [state1, state2]) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      broadcastMessage $ AddBorderOverride win 0
      sendMessage $ AddBorderOverride win 0
    when (action == remove || (action == toggle && isFull)) $ do
      broadcastMessage $ RemoveBorderOverride win
      sendMessage $ RemoveBorderOverride win
  return $ All True

fullscreenBorderEventHook _ = return $ All True
