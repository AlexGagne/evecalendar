{-# LANGUAGE OverloadedStrings #-}

module Eve.API
    (getUpcomingCalendarEvents
    ) where

import Control.Lens                 ((.~),(^.), (&))
import Data.ByteString.Lazy         (toStrict)
import Data.Text                    (Text, unpack, pack)
import Data.Text.Encoding           (decodeUtf8)
import Network.Wreq                 (param, getWith, defaults, responseBody)
import qualified System.Environment as E

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
                                                           & param (parameter option) .~ [value option]
  response <- getWith authOptions (eveBaseUrl ++ unpack path)
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


data Option = Option
  { parameter :: Text
  , value :: Text
  }




  {-
  getUpcomingCalendarEvents :: Int -> IO String
  getUpcomingCalendarEvents charId = getEveAPIRequest path
          where path = "/" ++ (show charId) ++ "/UpcomingCalendarEvents.xml.aspx"-}
