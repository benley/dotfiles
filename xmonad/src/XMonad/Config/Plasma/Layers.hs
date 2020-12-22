{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Config.Plasma.Layers where

import Control.Monad
import Data.Monoid
import qualified Data.Set as S

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Fullscreen
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Config.Plasma.Util


withLayers conf = conf
  { layoutHook = layersLayout . layoutHook $ conf
  , manageHook = composeAll [ manageLayers, manageHook conf ]
  , handleEventHook = composeAll [ layersEventHook, handleEventHook conf ]
  }


-- | Manage hook that adds new windows to 'LayersLayout'
manageLayers :: ManageHook
manageLayers = ask >>= \win -> do
  isNotification --> addWith AddNotification win
  isKdeOSD       --> addWith AddOSD          win
  isDock         --> addWith AddDock         win
  isDesktop      --> addWith AddDesktop      win
  isFullscreen   --> addWith AddFullscreen   win
  mempty
  where
    addWith msg win = liftX $ withDisplay $ \dpy -> do
      broadcastMessage $ msg win
      sendMessage $ msg win
      whenX (not <$> runQuery checkDock win) $
        io $ selectInput dpy win structureNotifyMask

-- | Event hook that removes destroyed windows from 'LayersLayout'
layersEventHook :: Event -> X All
layersEventHook (DestroyWindowEvent {ev_window = win}) = do
  forM_ [RemoveNotification, RemoveOSD, RemoveDock, RemoveDesktop] $ \msg -> do
    broadcastMessage $ msg win
    sendMessage $ msg win
  return (All True)
layersEventHook _ = return (All True)

data LayersMessage = AddNotification Window
                   | RemoveNotification Window
                   | AddOSD Window
                   | RemoveOSD Window
                   | AddDesktop Window
                   | RemoveDesktop Window
                   | AddDock Window
                   | RemoveDock Window


instance Message LayersMessage

-- | Layout modifier that places windows on different "layers"
-- in the following order:
--   * Desktop windows
--   * Tiled and floating windows
--   * Dock windows
--   * Notifications
--   * Fullscreen windows
--   * KDE on-screen-displays
-- (note: dock windows are above regular windows in kwin but we place them
--  below for covering docks when using X.H.ManageDocks)
layersLayout = ModifiedLayout (LayersLayout S.empty S.empty S.empty S.empty S.empty True)
data LayersLayout a = LayersLayout { notifications :: S.Set a
                                   , osds          :: S.Set a
                                   , desktops      :: S.Set a
                                   , bars          :: S.Set a
                                   , fullscreens   :: S.Set a
                                   , docksUpper    :: Bool } deriving (Read, Show)

instance LayoutModifier LayersLayout Window where
  hook (LayersLayout nset oset dset bset fset rdocks) = withDisplay $ \dpy -> do
    mapM_ (io . raiseWindow dpy) ((if rdocks then S.toList bset else []) ++ S.toList nset ++ S.toList fset ++ S.toList oset)
    mapM_ (io . lowerWindow dpy) ((if rdocks then [] else S.toList bset) ++ S.toList dset)

  pureMess ll@(LayersLayout nset oset dset bset fset rdocks) m
    | Just (AddNotification w) <- fromMessage m = if S.member w nset
                                    then Nothing
                                    else Just $ ll {notifications = S.insert w nset}
    | Just (RemoveNotification w) <- fromMessage m = if S.member w nset
                                       then Just $ ll {notifications = S.delete w nset}
                                       else Nothing
    | Just (AddOSD w) <- fromMessage m = if S.member w oset
                           then Nothing
                           else Just $ ll {osds = S.insert w oset}
    | Just (RemoveOSD w) <- fromMessage m = if S.member w oset
                              then Just $ ll {osds = S.delete w oset}
                              else Nothing
    | Just (AddDesktop w) <- fromMessage m = if S.member w dset
                               then Nothing
                               else Just $ ll {desktops = S.insert w dset}
    | Just (RemoveDesktop w) <- fromMessage m = if S.member w dset
                                  then Just $ ll {desktops = S.delete w dset}
                                  else Nothing
    | Just (AddDock w) <- fromMessage m = if S.member w bset
                            then Nothing
                            else Just $ ll {bars = S.insert w bset}
    | Just (RemoveDock w) <- fromMessage m = if S.member w bset
                                then Just $ ll {bars = S.delete w bset}
                                else Nothing
    | Just (AddFullscreen w) <- fromMessage m = if S.member w fset
                                  then Nothing
                                  else Just $ ll {fullscreens = S.insert w fset}
    | Just (RemoveFullscreen w) <- fromMessage m = if S.member w fset
                                      then Just $ ll {fullscreens = S.delete w fset}
                                      else Nothing
    | Just ToggleStruts <- fromMessage m = Just $ ll {docksUpper = not rdocks}
    | otherwise = Nothing
