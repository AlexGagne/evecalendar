module Eve.Types.CalendarEvent (CalendarEvent(..), Response(..))
where 

import Data.Text (Text)
import Data.Time (UTCTime)


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

data Response = Undecided | Accepted | Declined | Tentative deriving Show