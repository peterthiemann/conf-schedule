module ConfigData where

import BasicTypes

data Event
  = Event { evName :: String
          , evDate :: Date
          , evSessions :: [Session]
          , evEnd :: Time }
  deriving Show

data Session
  = Session { seName :: String
            , seChair :: Maybe String
            , seStart :: Time
            , seTalks :: [Talk] }
  deriving Show

data Talk
  = Talk { taName :: String
         , taStart :: Time
         , taAuthors :: [Authors] }
  deriving Show

data Authors
  = Authors { auNames :: [String]
            , auAffiliation :: String }
  deriving Show

