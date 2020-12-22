{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Layout.BorderConfiguration where

import Control.Monad
import Data.Monoid
import qualified Data.Map as M

import XMonad
import XMonad.Layout.LayoutModifier

data BorderMessage = AddBorderOverride Window Dimension
                   | RemoveBorderOverride Window

instance Message BorderMessage

manageBorders :: Dimension -> ManageHook
manageBorders bw = ask >>= \win -> liftX $ do
  sendMessage $ AddBorderOverride win bw
  broadcastMessage $ AddBorderOverride win bw
  mempty


setBorderEventHook :: Event -> X All
setBorderEventHook (DestroyWindowEvent {ev_window = win}) = do
  sendMessage $ RemoveBorderOverride win
  broadcastMessage $ RemoveBorderOverride win
  return (All True)
setBorderEventHook _ = return (All True)

borderLayout = ModifiedLayout (BorderLayout M.empty)
data BorderLayout a = BorderLayout (M.Map a Dimension) deriving (Read, Show)
instance LayoutModifier BorderLayout Window where
  hook (BorderLayout ovr) = withDisplay $ \dpy ->
    forM_ (M.toList ovr) $ \(w, bw) ->
      io $ setWindowBorderWidth dpy w bw

  pureMess (BorderLayout ovr) m
    | Just (AddBorderOverride w bw) <- fromMessage m = if w `M.member` ovr
                                                   then Nothing
                                                   else Just $ BorderLayout (M.insert w bw ovr)
    | Just (RemoveBorderOverride w) <- fromMessage m = if w `M.member` ovr
                                                   then Just $ BorderLayout (M.delete w ovr)
                                                   else Nothing
    | otherwise = Nothing

  handleMess bl m = do
    withDisplay $ \dpy -> case fromMessage m of
        Just (RemoveBorderOverride w) -> do
          bw <- asks (borderWidth . config)
          io $ setWindowBorderWidth dpy w bw
        _ -> return ()
    return $ pureMess bl m
