module System.Taffybar.Label (labelW) where

import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)
import Graphics.UI.Gtk (Widget, widgetShowAll)

labelW :: IO String -> IO Widget
labelW printer = do
  w <- pollingLabelNew "---" 1 printer
  widgetShowAll w
  return w
