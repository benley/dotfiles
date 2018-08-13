{-# LANGUAGE OverloadedStrings #-}
module Local.Taffybar.Widget.Label where

import Data.Text as T
import System.Taffybar.Context
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)
import qualified GI.Gtk

labelW :: IO Text -> TaffyIO GI.Gtk.Widget
labelW = pollTextW 60

pollTextW :: Double        -- ^ Poll interval in seconds
          -> IO Text       -- ^ Text
          -> TaffyIO GI.Gtk.Widget
pollTextW interval text = do
    l <- pollingLabelNew "" interval text
    GI.Gtk.widgetShowAll l
    GI.Gtk.toWidget l
    return l
