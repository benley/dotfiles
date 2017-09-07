import System.Taffybar

import System.Taffybar.Battery
import System.Taffybar.CPUMonitor
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.NetMonitor
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.Pager
import System.Taffybar.TaffyPager
import System.Taffybar.Weather
import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
--import System.Taffybar.WirelessMonitor

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

main = defaultTaffybar defaultTaffybarConfig
    { startWidgets = [ taffyPagerNew pagerConfig
                     --, notifyAreaNew defaultNotificationConfig
                     ]
    , endWidgets = [ textClockNew Nothing "<span fgcolor='#ea9560'>%a %b %_d %H:%M</span>" 1
                   , systrayNew
                   --, weatherNew (defaultWeatherConfig "BHBM3") 10
                   --, textBatteryNew "($time$)" 5
                   , batteryBarNew defaultBatteryConfig 5
                   --, wirelessMonitorNew 3 "wlp1s0"
                   , pollingGraphNew memCfg 1 memCallback
                   , cpuMonitorNew cpuCfg 1 "cpu0"
                   , cpuMonitorNew cpuCfg 1 "cpu1"
                   , netMonitorNew 1 "wlp1s0"
                   , mprisNew defaultMPRISConfig
                   ]
    , barHeight = 50
    }
