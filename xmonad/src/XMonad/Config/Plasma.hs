{-# LANGUAGE FlexibleContexts,FlexibleInstances,MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.Config.Plasma (
    kde5Config
    ) where

import           Data.Ratio    ((%))

import           XMonad
import qualified XMonad.StackSet              as W

import           XMonad.Config.Plasma.Layers
import           XMonad.Config.Plasma.Util

import           XMonad.Layout.BorderConfiguration

import           XMonad.Util.XUtils           (fi)

import XMonad.Config.DesktopCommon

kde5Config conf = desktop conf
    { layoutHook = layersLayout .
                   layoutHook $ conf
    , handleEventHook = composeAll [ layersEventHook
                                   , handleEventHook conf
                                   ]
    , manageHook = composeAll [ kdeNoBorders           --> manageBorders 0
                              , isKdeOSD               --> doPlaceKdeOSD
                              , isKdeOverride          --> doFloat
                              , manageLayers
                              , manageHook conf ]
    }

-- | Place Plasma OSD just like KWin does
doPlaceKdeOSD :: ManageHook
doPlaceKdeOSD = ask >>= \win -> do
  liftX $ moveWindowWith placeKdeOSD win
  doIgnore

moveWindowWith :: (W.RationalRect -> W.RationalRect) -> Window -> X ()
moveWindowWith f win = withDisplay $ \dpy -> do
    WindowAttributes wx wy ww wh bw _ _ _ _ <- io $ getWindowAttributes dpy win
    Rectangle sx sy sw sh <- gets (screenRect . W.screenDetail . W.current . windowset)
    let W.RationalRect rx ry rw rh = f $ W.RationalRect (fi (fi wx - sx) % fi sw)
                                                        (fi (fi wy - sy) % fi sh)
                                                        ((fi ww + 2 * fi bw) % fi sw)
                                                        ((fi wh + 2 * fi bw) % fi sh)
        x = fi sx + floor (fi sx + rx * fi sw)
        y = fi sy + floor (fi sy + ry * fi sh)
        w = floor (rw * fi sw)
        h = floor (rh * fi sh)
    tileWindow win (Rectangle x y w h)

placeKdeOSD (W.RationalRect _ _ w h) = W.RationalRect x y w h
  where x = 1/2 - w/2
        y = 2/3 - h/2
