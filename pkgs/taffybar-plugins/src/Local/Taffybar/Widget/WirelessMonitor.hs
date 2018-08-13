{-# LANGUAGE OverloadedStrings #-}
module Local.Taffybar.Widget.WirelessMonitor where

import Data.Text as T
import qualified GI.Gtk
import System.Taffybar.Context
import System.Taffybar.Widget.Generic.PollingLabel

import qualified Local.Network.IWlib as IW

wirelessMonitorNew :: Double -- ^ Polling interval (in seconds, e.g. 1.5)
                   -> String -- ^ Name of the interface to monitor
                   -> TaffyIO GI.Gtk.Widget
wirelessMonitorNew interval iface = do
    label <- pollingLabelNew "" interval (wifiCallback iface)
    GI.Gtk.widgetShowAll label
    GI.Gtk.toWidget label
    return label

wifiCallback :: String -> IO Text
wifiCallback iface = do
  wi <- IW.getWirelessInfo iface
  let essid_ = IW.wiEssid wi
      essid = if Prelude.null essid_ then "n/a" else essid_
      quality = IW.wiQuality wi
  return $ T.pack (show quality ++ "% " ++ essid)
