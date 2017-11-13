{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Local.Emoji where

import qualified Data.ByteString.Char8 as Char8
import Data.FileEmbed (embedFile)
import GHC.Generics
import qualified Data.Yaml as Y
import Data.Maybe (isNothing, fromJust)
import Data.Yaml (FromJSON(..), (.:), (.:?))

emoji "computer"                  = "\x1f4bb"
emoji "electric_plug"             = "\x1f50c"
emoji "fa-battery"                = "\xf240"
emoji "fa-battery-0"              = "\xf244"
emoji "fa-battery-1"              = "\xf243"
emoji "fa-battery-2"              = "\xf242"
emoji "fa-battery-3"              = "\xf241"
emoji "fa-battery-4"              = "\xf240"
emoji "fa-battery-empty"          = "\xf244"
emoji "fa-battery-full"           = "\xf240"
emoji "fa-battery-half"           = "\xf242"
emoji "fa-battery-quarter"        = "\xf243"
emoji "fa-battery-three-quarters" = "\xf241"
emoji "fa-bolt"                   = "\xf0e7"
emoji "fa-flash"                  = "\xf0e7"
emoji "fa-laptop"                 = "\xf109"
emoji "fa-microchip"              = "\xf2db"

embeddedData :: Char8.ByteString
embeddedData = $(embedFile "icons.yml")

iconList :: [IconInfo]
iconList =
    let decodedData = Y.decode embeddedData :: Maybe FontAwesomeInfo in
    case decodedData of Nothing -> error "wat"
                        Just FontAwesomeInfo{icons=i} -> i

lookupChars foo =
    filter isMatch iconList where
      isMatch x | Local.Emoji.id x == foo = True
                | isNothing (Local.Emoji.aliases x) = False
                | foo `elem` fromJust (Local.Emoji.aliases x) = True
                | otherwise = False

lookupUnicode :: String -> Maybe String
lookupUnicode name = do
    case lookupChars name of
      [] -> Nothing
      [x] -> Just [toEnum (read ("0x" ++ unicode x)) :: Char]
      _ -> error "multiple matches?"

data FontAwesomeInfo =
    FontAwesomeInfo { icons :: [IconInfo] }
    deriving (Show, Eq, Generic, FromJSON)

data IconInfo =
    IconInfo { name :: String
             --, filter :: Maybe [String]
             , unicode :: String
             -- , created :: Maybe String
             , id :: String
             --, categories :: Maybe [String]
             , aliases :: Maybe [String]
             }
    deriving (Show, Eq, Generic, FromJSON)
