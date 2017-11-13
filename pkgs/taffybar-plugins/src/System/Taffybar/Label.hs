module System.Taffybar.Label where

import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)
import qualified Graphics.UI.Gtk as Gtk

labelW :: IO String -> IO Gtk.Widget
labelW = pollTextW 60

pollTextW :: Double -- ^ Poll interval in seconds
          -> IO String -- ^ Text
          -> IO Gtk.Widget
pollTextW interval text = do
    l <- pollingLabelNew "" interval text
    Gtk.widgetShowAll l
    return l
