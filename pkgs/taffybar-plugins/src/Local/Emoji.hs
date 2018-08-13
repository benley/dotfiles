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
import qualified Data.Text as T

-- https://artyom.me/aeson#fromjson-instances-for-other-types

-- TODO: fetch icons.yml from the internet or something
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

-- individual non-FontAwesome symbols
-- TODO: this doesn't really belong here
emoji "computer"                  = "\x1f4bb"
emoji "electric_plug"             = "\x1f50c"
