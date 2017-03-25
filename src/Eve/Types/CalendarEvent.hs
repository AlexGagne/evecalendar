module Eve.Types.CalendarEvent (CalendarEvent(..), Response(..))
where 

import Data.Text (Text)
import Data.Time (UTCTime)

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