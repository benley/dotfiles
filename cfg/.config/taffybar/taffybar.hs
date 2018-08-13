-- -*- mode:haskell -*-
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isNumber, toUpper)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Safe (headMay)
import System.Process (readProcess)
import Text.Printf

import System.Taffybar
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.PollingLabel ()
import System.Taffybar.Widget.Text.NetworkMonitor (networkMonitorNew)
import System.Taffybar.Widget.Util ()
import System.Taffybar.Widget.Workspaces ()
import System.Taffybar.Widget.XDGMenu.MenuWidget (menuWidgetNew)

import Local.Emoji (lookupUnicode, emoji)
import Local.Taffybar.Widget.Label
import Local.Taffybar.Widget.WirelessMonitor

transparent = (0.0, 0.0, 0.0, 0.0)
green1 = (0, 1, 0, 1)
green2 = (1, 0, 1, 0.5)
taffyBlue = (0.129, 0.588, 0.953, 1)
red = (1, 0, 0, 1)

myGraphConfig :: GraphConfig
myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 75
  , graphBackgroundColor = transparent
  , graphDirection = RIGHT_TO_LEFT
  }

netCfg :: GraphConfig
netCfg = myGraphConfig
  { graphDataColors = [red, green2]
  , graphLabel = Just "net"
  }

memCfg :: GraphConfig
memCfg = myGraphConfig
  { graphDataColors = [taffyBlue]
  , graphLabel = Just (T.pack (fontAwesome "microchip"))
  }

cpuCfg :: GraphConfig
cpuCfg = myGraphConfig
  { graphDataColors = [green1, green2]
  , graphLabel = Just $ T.pack $ emoji "computer"
  }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

separator = labelW (return "|")

fontAwesome :: String -> String
fontAwesome n =
    case lookupUnicode n of
      Nothing -> error "invalid character"
      Just x -> "<span font_desc='FontAwesome'>" ++ x ++ "</span>"

acStatus :: IO T.Text
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
    case online of "yes" -> return $ T.pack $ fontAwesome "plug"
                   _ -> return ""

batString :: IO T.Text
batString = do
    batList <- fmap (headMay . filter (isInfixOf "battery") . lines)
             . readProcess "upower" ["-e"]
             $ []
    batInfo <- readProcess "upower" ["-i", fromMaybe "" batList] []

    let batPercent = takeWhile (/= '%')
                   . filter (/= ' ')
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
                   -- what does state "UNKNOWN" mean?
                   | otherwise = False

    return $ if null batPercent
               then "?????" -- no battery
               else T.pack $ batteryIcon (read batPercent) isCharging

batteryIcon :: Int -> Bool -> String
batteryIcon batP isCharging
    | batP > 90 =
        chargeSym ++ fontAwesome "battery-full" ++ show batP ++ "%"
    | batP > 60 =
        chargeSym ++ fontAwesome "battery-three-quarters" ++ show batP ++ "%"
    | batP > 40 =
        chargeSym ++ fontAwesome "battery-half" ++ show batP ++ "%"
    | batP > 10 =
        chargeSym ++ fontAwesome "battery-quarter" ++ show batP ++ "%"
    | batP > 6  =
        colorIfDischarging "#ec5f67" ""
            $ chargeSym ++ fontAwesome "battery-empty" ++ show batP ++ "%"
    | otherwise =
        colorIfDischarging "#000000" "#ec5f67"
            $ chargeSym ++ fontAwesome "battery-empty" ++ show batP ++ "%"
  where
    colorIfDischarging fg bg = if isCharging then id else colorize fg bg
    chargeSym = if isCharging then fontAwesome "bolt" else ""

main :: IO ()
main =
  let myWorkspacesWidget = workspacesNew $ defaultWorkspacesConfig
        { minIcons = 0
        , widgetGap = 0
        -- , showWorkspaceFn = hideEmpty
        }
      myWindowsWidget = windowsNew $ defaultWindowsConfig
        { getActiveLabel = truncatedGetActiveLabel 80
        }
      myMenuWidget = menuWidgetNew $ Just "gnome-"
  in dyreTaffybar $
     withLogServer $
     withToggleServer $
     toTaffyConfig $
     defaultSimpleTaffyConfig
        { startWidgets = map (>>= buildContentsBox)
          [ myMenuWidget
          , myWorkspacesWidget
          ]
        , centerWidgets = map (>>= buildContentsBox)
          [ myWindowsWidget
          ]
        , endWidgets = map (>>= buildContentsBox)
          [ textClockNew Nothing (colorize "#ea9560" "" "%a %b %_d %H:%M") 1
          , sniTrayNew  -- This expects that you have a status-notifier-watcher running already
          , separator
          , pollTextW 15 batString
          , pollTextW 5 acStatus
          , separator
          , wirelessMonitorNew 3 "wlp2s0"
          , separator
          , pollingGraphNew cpuCfg 1 cpuCallback
          , pollingGraphNew memCfg 1 memCallback
          , separator
          -- , networkGraphNew netCfg Nothing
          , networkMonitorNew netMonitorFormat networkInterfaces
          ]
        , barHeight = 55
        , barPadding = 0
        -- , barPosition = Top   -- Top is the default
        , widgetSpacing = 0  -- control padding via taffybar.css
        -- , getMonitorConfig = allMonitors
        }

netMonitorFormat :: String
netMonitorFormat =
  printf "<span size=\"smaller\">%s KiB/s\n%s KiB/s</span>"
  (colorize "#FF0000" "" "▲ $outKB$")
  (colorize "#00FF00" "" "▼ $inKB$")

networkInterfaces :: Maybe [String]
networkInterfaces = Just ["wlp2s0", "enp0s31f6"]
