{-# LANGUAGE MultiParamTypeClasses  #-}

module Eve.Internal.Types.CalendarEvent (CalendarEvents(..), CalendarEvent(..), Response(..)) where

import Data.Text                      (Text)
import Data.Time                      (UTCTime)
import Eve.Internal.Types.CachedUntil (CachedUntil, cachedUntil)

-- | 'CalendarEvents' represents all the calendar events with the cached timer
data CalendarEvents = CalendarEvents
  { calendarEvents :: [CalendarEvent]
  , _calendarCachedUntil :: UTCTime
  } deriving Show

instance CachedUntil CalendarEvents UTCTime where cachedUntil = _calendarCachedUntil


-- | 'CalendarEvent' represents the Calendar data from EVE's XML API.
data CalendarEvent = CalendarEvent
  { eventID :: Int
  , ownerID :: Int
  , ownerName :: Text
  , eventDate :: UTCTime -- format: 2017-03-26 00:00:00
  , eventTitle :: Text
  , duration :: Int --duration is in minutes
  , importance :: Bool
  , response :: Response
  , eventText :: Text
  } deriving Show

-- | 'Response' represents the different states a response to a calendar event can be.
data Response = Undecided | Accepted | Declined | Tentative deriving Show