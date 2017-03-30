{-# LANGUAGE OverloadedStrings #-}

module Eve.Internal.Utils.XmlReader (xmlToCalendarEvents, xmlToCharacters, getCacheTimerFromXml) 
    where

import Data.Maybe                   (fromJust, isJust, fromMaybe)
import Data.Text                    (Text, pack, unpack)
import Data.Time                    (UTCTime)
import Text.XML.Light.Input         (parseXML)
import Text.XML.Light.Proc          (findElements, onlyElems, findAttr, strContent)
import Text.XML.Light.Types         (Content, Element, QName (QName), elContent)

import Eve.Internal.Utils.Utilities (fromJust', textToUTCTime)
import Eve.Types                    (CalendarEvent(..), Character(..), Response(..))

xmlToCalendarEvents :: Text -> [CalendarEvent]
xmlToCalendarEvents xml = map calendarEventFromElement calendarElements
  where
    xmlDocument = parseXML xml
    calendarElements = concatMap (findElements $ simpleQName "row") (onlyElems xmlDocument)

xmlToCharacters :: Text -> [Character]
xmlToCharacters xml = map characterFromElement characterElements
  where
    xmlDocument = parseXML xml
    characterElements = getRows xmlDocument

getCacheTimerFromXml :: Text -> UTCTime
getCacheTimerFromXml xml = textToUTCTime $ pack cachedUntil
  where 
    xmlDocument = parseXML xml
    cachedUntil = strContent $ head $ concatMap (findElements $ simpleQName "cachedUntil") (onlyElems xmlDocument)

getRows :: [Content] -> [Element]
getRows xmlDocument = concatMap (findElements $ simpleQName "row") (onlyElems xmlDocument)

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
getBoolFromElement name element = getIntFromElement name element == 1

getUTCTimeFromElement :: Text -> Element -> UTCTime
getUTCTimeFromElement name element = textToUTCTime dateString where
  dateString = pack $ getStringFromElement name element

getIntFromElement :: Text -> Element -> Int
getIntFromElement name element = read (getStringFromElement name element) :: Int

getTextFromElement :: Text -> Element -> Text
getTextFromElement name element = pack $ getStringFromElement name element

getStringFromElement :: Text -> Element -> String
getStringFromElement name element = fromJust' ((findAttr $ simpleQName name) element) $ unpack name

simpleQName :: Text -> QName
simpleQName name = QName (unpack name) Nothing Nothing

