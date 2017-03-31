{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Eve.Internal.Utils.XmlReader
Description : The module contains all the utility functions to process xml
Copyright   : (c) Alex Gagné, 2017
License     : MIT
Stability   : experimental

This module contains all the utility functions to read xml from a Text and export it into the various data types used by the library

-}

module Eve.Internal.Utils.XmlReader (xmlToCalendarEvents, xmlToCharacters) 
    where

import Data.Maybe                   (fromJust, isJust, fromMaybe)
import Data.Text                    (Text, pack, unpack)
import Data.Time                    (UTCTime)
import Text.XML.Light.Input         (parseXML)
import Text.XML.Light.Proc          (findElements, onlyElems, findAttr, strContent)
import Text.XML.Light.Types         (Content, Element, QName (QName), elContent)

import Eve.Internal.Utils.Utilities (fromJust', textToUTCTime)
import Eve.Types                    (CalendarEvents(..), CalendarEvent(..), Response(..), Characters(..), Character(..))

{- | Transforms an XML string into a CalendarEvents. Assumes that the XML string
     is the usual format returned by EVE Online's XML API
-}
xmlToCalendarEvents :: Text -> CalendarEvents
xmlToCalendarEvents xml = CalendarEvents calendarEvents cacheTimer
  where
    calendarEvents = xmlToCalendarEventArray xml
    cacheTimer = getCacheTimerFromXml xml

{- | Transforms an XML string into a Characters. Assumes that the XML string
     is the usual format returned by EVE Online's XML API
-}
xmlToCharacters :: Text -> Characters
xmlToCharacters xml = Characters characters cacheTimer
  where
    characters = xmlToCharacterArray xml
    cacheTimer = getCacheTimerFromXml xml

xmlToCalendarEventArray :: Text -> [CalendarEvent]
xmlToCalendarEventArray xml = map calendarEventFromElement calendarElements
  where
    xmlDocument = parseXML xml
    calendarElements = concatMap (findElements $ simpleQName "row") (onlyElems xmlDocument)

xmlToCharacterArray :: Text -> [Character]
xmlToCharacterArray xml = map characterFromElement characterElements
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

-- We assume here that 1 = True and 0 = False in the element, from Eve Online's XML API
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

