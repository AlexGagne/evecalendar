{-# LANGUAGE OverloadedStrings #-}

module EveAPI
    (getUpcomingCalendarEvents
    ) where

import Control.Lens                 ((.~),(^.), (&))
import Data.ByteString.Lazy         (toStrict)
import Data.Time                    (UTCTime)
import Data.Time.Format             (parseTimeOrError, defaultTimeLocale)
import Data.Maybe                   (fromJust, isJust)
import Data.Text                    (Text, unpack, pack)
import Data.Text.Encoding           (decodeUtf8)
import Network.Wreq                 (param, getWith, defaults, responseBody)
import qualified System.Environment as E
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types

getUpcomingCalendarEvents :: IO [CalendarEvent]
getUpcomingCalendarEvents = do
  putStrLn "Geting character XML"
  eveCharacterXML <- getCharacterXML
  putStrLn "Got character XML"
  -- We only take the first character's event
  let charId = characterID $ head $ xmlToCharacters eveCharacterXML
  let path = "/char/UpcomingCalendarEvents.xml.aspx"
  let parameter = pack "characterID"
  let paramValue = pack (show charId)
  putStrLn "Geting calendar events XML"
  calendarEventXml <- getEveAPIRequest path (Just $ Option parameter paramValue)
  putStrLn "Got calendar events XML"
  return $ xmlToCharacterEvents calendarEventXml

xmlToCharacterEvents :: Text -> [CalendarEvent]
xmlToCharacterEvents xml = map calendarEventFromElement characterElements
  where
    xmlDocument = parseXML xml
    characterElements = concatMap (findElements $ simpleQName "row") (onlyElems xmlDocument)

xmlToCharacters :: Text -> [Character]
xmlToCharacters xml = map characterFromElement characterElements
  where
    xmlDocument = parseXML xml
    characterElements = concatMap (findElements $ simpleQName "row") (onlyElems xmlDocument)

getCharacterXML :: IO Text
getCharacterXML = getEveAPIRequest "/account/Characters.xml.aspx" Nothing

-- Deal with errors
getEveAPIRequest :: Text -> Maybe Option -> IO Text
getEveAPIRequest path option = do
  keyID <- getKeyID
  vCode <- getVCode
  let authOptions = case option of Nothing -> defaults & param "keyID" .~ [keyID]
                                                       & param "vCode" .~ [vCode]
                                   Just option -> defaults & param "keyID" .~ [keyID]
                                                           & param "vCode" .~ [vCode]
                                                           & param (parameter option) .~ [(value option)]
  response <- getWith authOptions (eveBaseUrl ++ (unpack path))
  return $ decodeUtf8 . toStrict $ response ^. responseBody
  where eveBaseUrl = "https://api.eveonline.com"

getKeyID :: IO Text
getKeyID = do
  keyID <- E.getEnv "KEY_ID"
  return $ pack keyID

getVCode :: IO Text
getVCode = do
  vCode <- E.getEnv "V_CODE"
  return $ pack vCode

simpleQName :: Text -> QName
simpleQName name = QName (unpack name) Nothing Nothing

-- TODO deal with Maybe
characterFromElement :: Element -> Character
characterFromElement element = Character name characterID corporationName corporationID allianceName allianceID factionName factionID
  where
    name = pack $ fromJust $ (findAttr $ simpleQName "name") element
    characterID = read (fromJust $ (findAttr $ simpleQName "characterID") element) :: Int
    corporationName = pack $ fromJust $ (findAttr $ simpleQName "corporationName") element
    corporationID = read(fromJust $ (findAttr $ simpleQName "corporationID") element) :: Int
    allianceName = pack $ fromJust $ (findAttr $ simpleQName "allianceName") element
    allianceID = read (fromJust $ (findAttr $ simpleQName "allianceID") element) :: Int
    factionName = pack $ fromJust $ (findAttr $ simpleQName "factionName") element
    factionID =  read (fromJust $ (findAttr $ simpleQName "factionID") element) :: Int

calendarEventFromElement :: Element -> CalendarEvent
calendarEventFromElement element = CalendarEvent eventID ownerID ownerName eventDate eventTitle duration importance response eventText
  where
    eventID = read (fromJust' ((findAttr $ simpleQName "eventID") element) "eventID") :: Int
    ownerID = read (fromJust' ((findAttr $ simpleQName "ownerID") element) "ownerID") :: Int
    ownerName = pack $ fromJust' ((findAttr $ simpleQName "ownerName") element) "ownerName"
    dateString = fromJust' ((findAttr $ simpleQName "eventDate") element) "eventDate1"
    eventDate = (parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" dateString) :: UTCTime
    eventTitle = pack $ fromJust $ (findAttr $ simpleQName "eventTitle") element
    duration = read (fromJust $ (findAttr $ simpleQName "duration") element) :: Int
    importance = if (read (fromJust $ (findAttr $ simpleQName "duration") element) :: Int) == 1 then True else False
    responseString = fromJust $ (findAttr $ simpleQName "eventText") element
    response =  case responseString of
                  "Undecided" -> Undecided
                  "Accepted" -> Accepted
                  "Declined" -> Declined
                  "Tentative" -> Tentative
                  _ -> Undecided
    eventText = pack $ fromJust $ (findAttr $ simpleQName "eventText") element

-- Will throw error err if it is Nothing
fromJust':: Maybe a -> String -> a
fromJust' m err = case m of
    Nothing -> error err
    Just m -> m

data Option = Option
  { parameter :: Text
  , value :: Text
  }

data Character = Character
  { name :: Text
  , characterID :: Int
  , corporationName :: Text
  , corporationID :: Int
  , allianceName :: Text
  , allianceID :: Int
  , factionName :: Text
  , factionID :: Int
  }

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

  {-
  getUpcomingCalendarEvents :: Int -> IO String
  getUpcomingCalendarEvents charId = getEveAPIRequest path
          where path = "/" ++ (show charId) ++ "/UpcomingCalendarEvents.xml.aspx"-}
