import System.Taffybar

import System.Taffybar.Battery
import System.Taffybar.CPUMonitor
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.Label
import System.Taffybar.NetMonitor
--import System.Taffybar.Menu.MenuWidget
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

import Data.Char (isNumber, toUpper)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Safe (headMay)
import System.Process (readProcess)

import Local.Emoji (lookupUnicode)

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

memCfg =
    defaultGraphConfig
    { graphDataColors = [(1, 0, 0, 1)]
    -- , graphLabel = Just "\xf2db"
    }

cpuCfg =
    defaultGraphConfig
    { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)] }

pagerConfig =
    defaultPagerConfig
    { activeWorkspace = colorize "#263238" "#89DDFF" . wrap " " " " . escape
    , widgetSep = " | " }

separator = labelW (return "|")

fontAwesome :: String -> String
fontAwesome n =
    case lookupUnicode n of
      Nothing -> error "invalid character"
      Just x -> "<span font_desc='FontAwesome'>" ++ x ++ "</span>"

acStatus :: IO String
acStatus = do
    devList <- fmap (headMay . filter (isInfixOf "line_power") . lines)
             . readProcess "upower" ["-e"]
             $ []
    acInfo <- readProcess "upower" ["-i", fromMaybe "" devList] []
    let online = last
               . words
               . fromMaybe ": ?"
               . headMay
               . filter (isInfixOf " online: ")
               . lines
               $ acInfo
    case online of "yes" -> return $ fontAwesome "plug"
                   _ -> return ""

batString :: IO String
batString = do
    batList <- fmap (headMay . filter (isInfixOf "battery") . lines)
             . readProcess "upower" ["-e"]
             $ []
    batInfo <- readProcess "upower" ["-i", fromMaybe "" batList] []

    let batPercent = filter (/= ' ')
                   . dropWhile (not . isNumber)
                   . fromMaybe ""
                   . headMay
                   . filter (isInfixOf "percentage:")
                   . lines
                   $ batInfo
        batState = fmap toUpper
                 . filter (/= ' ')
                 . dropWhile (/= ' ')
                 . dropWhile (== ' ')
                 . fromMaybe ""
                 . headMay
                 . filter (isInfixOf "state:")
                 . lines
                 $ batInfo
        isCharging | "DISCHARGING" `isInfixOf` batState = False
                   | "CHARGING" `isInfixOf` batState = True
                   -- what does state "UNKNOWN"mean?
                   | otherwise = False

    return $ batteryIcon batPercent isCharging

batteryIcon :: String -> Bool -> String
batteryIcon percent isCharging
    | bat > 90 =
        chargeSym ++ fontAwesome "battery-full" ++ show bat ++ "%"
    | bat > 60 =
        chargeSym ++ fontAwesome "battery-three-quarters" ++ show bat ++ "%"
    | bat > 40 =
        chargeSym ++ fontAwesome "battery-half" ++ show bat ++ "%"
    | bat > 10 =
        chargeSym ++ fontAwesome "battery-quarter" ++ show bat ++ "%"
    | bat > 6  =
        colorIfDischarging "#ec5f67" ""
            $ chargeSym ++ fontAwesome "battery-empty" ++ show bat ++ "%"
    | otherwise =
        colorIfDischarging "#000000" "#ec5f67"
            $ chargeSym ++ fontAwesome "battery-empty" ++ show bat ++ "%"
  where
    bat = read . reverse . takeWhile (/= ' ') . drop 1 . dropWhile (/= '%') . reverse $ percent
    colorIfDischarging fg bg = if isCharging then id else colorize fg bg
    chargeSym = if isCharging then fontAwesome "bolt" else ""


main = defaultTaffybar defaultTaffybarConfig
    { startWidgets = [ taffyPagerNew pagerConfig
                     --, notifyAreaNew defaultNotificationConfig
                     ]
    , endWidgets = [ textClockNew Nothing "<span fgcolor='#ea9560'>%a %b %_d %H:%M</span>" 1
                   , systrayNew
                   --, weatherNew (defaultWeatherConfig "BHBM3") 10
                   --, separator
                   --, textBatteryNew "($time$)" 15
                   , pollTextW 15 batString
                   , pollTextW 5 acStatus
                   , separator
                   , wirelessMonitorNew 3 "wlp1s0"
                   , separator
                   , labelW (return $ fontAwesome "microchip")
                   , pollingGraphNew memCfg 1 memCallback
                   , labelW (return "\x1f4bb")
                   , cpuMonitorNew cpuCfg 1 "cpu3"
                   , cpuMonitorNew cpuCfg 1 "cpu2"
                   , cpuMonitorNew cpuCfg 1 "cpu1"
                   , cpuMonitorNew cpuCfg 1 "cpu0"
                   , separator
                   , netMonitorNew 1 "wlp1s0"
                   --, mprisNew defaultMPRISConfig
                   --, menuWidgetNew Nothing
                   ]
    , barHeight = 50
    }
