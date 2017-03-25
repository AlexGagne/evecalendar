{-# LANGUAGE OverloadedStrings #-}

module Eve.API
    (getUpcomingCalendarEvents
    ) where

import Control.Lens                 ((.~),(^.), (&))
import Data.ByteString.Lazy         (toStrict)
import Data.Text                    (Text, unpack, pack)
import Data.Text.Encoding           (decodeUtf8)
import Network.Wreq                 (param, getWith, defaults, responseBody)
import Network.Wreq.Types           (params)
import qualified System.Environment as E

import Eve.Types                    (CalendarEvent, Character, characterID)
import Eve.Utils.XmlReader          (xmlToCalendarEvents, xmlToCharacters)

getUpcomingCalendarEvents :: IO [CalendarEvent]
getUpcomingCalendarEvents = do
  eveCharacterXML <- getCharacterXML
  -- We only take the first character's event for now
  let charID = characterID $ head $ xmlToCharacters eveCharacterXML
  calendarEventXml <- getCalendarXML charID
  return $ xmlToCalendarEvents calendarEventXml

getCharacterXML :: IO Text
getCharacterXML = getEveAPIRequest "/account/Characters.xml.aspx" [] 

getCalendarXML :: Int -> IO Text
getCalendarXML charID = getEveAPIRequest "/char/UpcomingCalendarEvents.xml.aspx" [("characterID", pack (show charID))] 

-- TODO: Deal with errors
getEveAPIRequest :: Text ->  [(Text, Text)] -> IO Text
getEveAPIRequest path options = do
  keyID <- getKeyID
  vCode <- getVCode
  let authOptions = defaults {params = [("keyId", keyID), ("vCode", vCode)] ++ options} 
  response <- getWith authOptions (eveBaseUrl ++ unpack path)
  return $ decodeUtf8 . toStrict $ response ^. responseBody
  where 
    eveBaseUrl = "https://api.eveonline.com"

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