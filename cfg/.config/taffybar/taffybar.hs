import System.Taffybar

import System.Taffybar.Battery
import System.Taffybar.CPUMonitor
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.Label
import System.Taffybar.NetMonitor
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.Pager
import System.Taffybar.TaffyPager
import System.Taffybar.Weather
import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.WirelessMonitor

import System.Information.Memory
import System.Information.CPU

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

memCfg =
    defaultGraphConfig
    { graphDataColors = [(1, 0, 0, 1)]
    , graphLabel = Just "mem"
    }

cpuCfg =
    defaultGraphConfig
    { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)] }

pagerConfig =
    defaultPagerConfig
    { activeWorkspace = colorize "#263238" "#89DDFF" . wrap " " " " . escape }

separator = labelW (return "|")

-- | Get the correct icon for the battery
-- batteryIcon :: String -> String
-- batteryIcon x
--     | bat > 90  = fontAwesome "\xf240  " ++ show bat ++ "%"
--     | bat > 60  = fontAwesome "\xf241  " ++ show bat ++ "%"
--     | bat > 40  = fontAwesome "\xf242  " ++ show bat ++ "%"
--     | bat > 10  = fontAwesome "\xf243  " ++ show bat ++ "%"
--     | otherwise = fontAwesome "\xf244  " ++ show bat ++ "%"
--   where
--     bat = read . reverse . takeWhile (/= ' ') . drop 1 . dropWhile (/= '%') . reverse $ x

main = defaultTaffybar defaultTaffybarConfig
    { startWidgets = [ taffyPagerNew pagerConfig
                     --, notifyAreaNew defaultNotificationConfig
                     ]
    , endWidgets = [ textClockNew Nothing "<span fgcolor='#ea9560'>%a %b %_d %H:%M</span>" 1
                   , systrayNew
                   --, weatherNew (defaultWeatherConfig "BHBM3") 10
                   , separator
                   , textBatteryNew "($time$)" 5
                   , separator
                   , batteryBarNew defaultBatteryConfig 5
                   , separator
                   , wirelessMonitorNew 3 "wlp1s0"
                   , separator
                   , pollingGraphNew memCfg 1 memCallback
                   , labelW (return  "\x1F4BB|")
                   , cpuMonitorNew cpuCfg 1 "cpu0"
                   , cpuMonitorNew cpuCfg 1 "cpu1"
                   , separator
                   , netMonitorNew 1 "wlp1s0"
                   , mprisNew defaultMPRISConfig
                   ]
    , barHeight = 50
    }
