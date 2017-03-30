{-# LANGUAGE MultiParamTypeClasses  #-}

module Eve.Internal.Types.Character (Characters(..), Character(..))
where 

import Data.Text                      (Text)
import Data.Time                      (UTCTime)
import Eve.Internal.Types.CachedUntil (CachedUntil, cachedUntil)

-- | 'CalendarEvents' represents all the calendar events with the cached timer
data Characters = Characters
  { characters :: [Character]
  , _characterCachedUntil :: UTCTime
  } deriving Show

instance CachedUntil Characters UTCTime where cachedUntil = _characterCachedUntil

-- | 'Character' represents the Character data from EVE's XML API.
data Character = Character
  { name :: Text
  , characterID :: Int
  , corporationName :: Text
  , corporationID :: Int
  , allianceName :: Text
  , allianceID :: Int
  , factionName :: Text
  , factionID :: Int
  } deriving Show