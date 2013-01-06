{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module BasicTypes where

import Data.Aeson
import Data.Aeson.TH

import Control.Applicative ((<$>), (<*>), pure)

data Month =
  Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec 
      deriving (Enum, Bounded, Eq, Show, Ord)

data Date =
  Date Month Int Int
  deriving (Show, Eq)

data Time =
  Time Int Int
  deriving (Show, Eq)

-- json instances
instance FromJSON Date where
    parseJSON (Object v) = Date <$>
          v .: "month" <*>
          v .: "day" <*>
          v .: "year"
    parseJSON _ = fail "Could not parse date..."

instance ToJSON Date where
    toJSON (Date m d y)
        = object [ "month" .= m
                 , "day" .= d
                 , "year" .= y
                 ]

instance FromJSON Time where
    parseJSON (Object v) = Time <$>
          v .: "h" <*>
          v .: "m"
    parseJSON _ = fail "Could not parse date..."

instance ToJSON Time where
    toJSON (Time h m)
        = object [ "h" .= h
                 , "m" .= m
                 ]

instance FromJSON Month where
    parseJSON (String value) =
        case value of
          "Jan" -> pure Jan
          "Feb" -> pure Feb
          "Mar" -> pure Mar
          "Apr" -> pure Apr
          "May" -> pure May
          "Jun" -> pure Jun
          "Jul" -> pure Jul
          "Aug" -> pure Aug
          "Sep" -> pure Sep
          "Oct" -> pure Oct
          "Nov" -> pure Nov
          "Dec" -> pure Dec
          _ -> fail "Invalid month"

    parseJSON _ = fail "Invalid month"

instance ToJSON Month where
    toJSON value = case value of
                     Jan -> "Jan"
                     Feb -> "Feb"
                     Mar -> "Mar"
                     Apr -> "Apr"
                     May -> "May"
                     Jun -> "Jun"
                     Jul -> "Jul"
                     Aug -> "Aug"
                     Sep -> "Sep"
                     Oct -> "Oct"
                     Nov -> "Nov"
                     Dec -> "Dec"


