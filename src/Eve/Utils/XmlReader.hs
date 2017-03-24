module Eve.Utils.XmlReader () 
    where

import Data.Maybe                   (fromJust, isJust, fromMaybe)
import Data.Text
import Data.Time (UTCTime)
import Data.Time.Format             (parseTimeOrError, defaultTimeLocale)
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types

import Eve.Utils.Utilities          (fromJust')
import Eve.Types                    (CalendarEvent, Response, Character)

characterFromElement :: Element -> Character
characterFromElement element = 
    Character name characterID corporationName corporationID allianceName allianceID factionName factionID
        where
            name = getTextFromElement "name" element
            characterID = getIntFromElement "characterID" element
            corporationName = getTextFromElement "corporationName" element
            corporationID = getIntFromElement "corporationID" element
            allianceName = getTextFromElement "allianceName" element
            allianceID = getIntFromElement "allianceID" element
            factionName = getTextFromElement "factionName" element
            factionID =  getIntFromElement "factionID" element

calendarEventFromElement :: Element -> CalendarEvent
calendarEventFromElement element = 
    CalendarEvent eventID ownerID ownerName eventDate eventTitle duration importance response eventText
        where
            eventID = getIntFromElement "eventID" element
            ownerID = getIntFromElement "ownerID" element
            ownerName = getTextFromElement "ownerName" element
            eventDate = getUTCTimeFromElement "eventDate" element
            eventTitle = getTextFromElement "eventTitle" element
            duration = getIntFromElement "duration" element
            importance = getBoolFromElement "importance" element
            response = getResponseFromElement "response" element
            eventText = getTextFromElement "eventText" element


getResponseFromElement :: Text -> Element -> Response
getResponseFromElement name element = 
    case responseString of
        "Undecided" -> Undecided
        "Accepted" -> Accepted
        "Declined" -> Declined
        "Tentative" -> Tentative
        _ -> Undecided
    where
        responseString = getStringFromElement name element

-- We assume here that 1 = True and 0 = False in the element
getBoolFromElement :: Text -> Element -> Bool
getBoolFromElement name element = (getIntFromElement name element) == 1

getUTCTimeFromElement :: Text -> Element -> UTCTime
getUTCTimeFromElement name element = let dateString = getStringFromElement name element in
    parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" dateString :: UTCTime

getIntFromElement :: Text -> Element -> Int
getIntFromElement name element = read (getStringFromElement name element) :: Int

getTextFromElement :: Text -> Element -> Text
getTextFromElement name element = pack $ getStringFromElement name element

getStringFromElement :: Text -> Element -> String
getStringFromElement name element = fromJust' ((findAttr $ simpleQName name) element) name

simpleQName :: Text -> QName
simpleQName name = QName (unpack name) Nothing Nothing

