{-# LANGUAGE MultiParamTypeClasses  #-}

{-|
Module      : Eve.Internal.Types.Character
Description : The module contains the data and related functions to the Characters
Copyright   : (c) Alex Gagné, 2017
License     : MIT
Stability   : experimental
-}

module Eve.Internal.Types.Character (Characters(..), Character(..))
where 

import Data.Text                      (Text)
import Data.Time                      (UTCTime)
import Eve.Internal.Types.CachedUntil (CachedUntil, cachedUntil)

-- | 'Characters' represents all the character events with the cached timer
data Characters = Characters
  { characters :: [Character]
  , _characterCachedUntil :: UTCTime
  } deriving Show

-- | 'cachedUntil' will fetch the cache timer on the characters
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