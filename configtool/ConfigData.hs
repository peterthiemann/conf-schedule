{-# LANGUAGE TemplateHaskell #-}

module ConfigData where

import BasicTypes

import Data.Aeson
import Data.Aeson.TH

data Event
  = Event { evName :: String
          , evDate :: Date
          , evRoom :: String
          , evSessions :: [Session]
          , evEnd :: Time }
  deriving (Show, Eq)

data Session
  = Session { seName :: String
            , seChair :: Maybe String
            , seStart :: Time
            , seTalks :: [Talk] }
  deriving (Show, Eq)

data Talk
  = Talk { taName :: String
         , taStart :: Time
         , taAuthors :: [Authors] }
  deriving (Show, Eq)

data Authors
  = Authors { auNames :: [String]
            , auAffiliation :: String }
  deriving (Show, Eq)

-- json instances
deriveJSON (drop 2) ''Event
deriveJSON (drop 2) ''Session
deriveJSON (drop 2) ''Talk
deriveJSON (drop 2) ''Authors

