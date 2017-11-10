module System.Taffybar.WirelessMonitor where

import qualified Graphics.UI.Gtk as Gtk
import qualified System.Information.IWlib as IW
import System.Taffybar.Widgets.PollingLabel

wirelessMonitorNew :: Double -- ^ Polling interval (in seconds, e.g. 1.5)
                   -> String -- ^ Name of the interface to monitor
                   -> IO Gtk.Widget
wirelessMonitorNew interval iface = do
    label <- pollingLabelNew "" interval (wifiCallback iface)
    Gtk.widgetShowAll label
    return (Gtk.toWidget label)

wifiCallback :: String -> IO String
wifiCallback iface = do
  wi <- IW.getWirelessInfo iface
  let essid_ = IW.wiEssid wi
      essid = if null essid_ then "n/a" else essid_
      quality = IW.wiQuality wi
  return (show quality ++ "% " ++ essid)
