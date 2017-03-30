{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Eve.API
Description : The module containing all the function calls to get Eve's XML Api
Copyright   : (c) Alex Gagné, 2017
License     : MIT

The module containing all the function calls to get EVE's XML Api.
You must include your API key and verification code in an Environment Variable.
-}
module Eve.API
    (getUpcomingCalendarEvents, getCharacters
    ) where

import Control.Lens                 ((.~),(^.), (&))
import Data.ByteString.Lazy         (toStrict)
import Data.Text                    (Text, unpack, pack)
import Data.Text.Encoding           (decodeUtf8)
import Network.Wreq                 (param, getWith, defaults, responseBody)
import Network.Wreq.Types           (params)
import qualified System.Environment as E
import Data.Functor.Identity

import Eve.Types                    (CalendarEvent, Character, characterID)
import Eve.Internal.Utils.XmlReader (xmlToCalendarEvents, xmlToCharacters, getCacheTimerFromXml)

-- | Fetches all the calendar events from EVE's XML API
getUpcomingCalendarEvents :: IO [CalendarEvent]
getUpcomingCalendarEvents = do
  characters <- getCharacters
  -- We only take the first character's calendar for now
  let charID = characterID $ head characters
  calendarEventXml <- getCalendarXML charID
  return $ xmlToCalendarEvents calendarEventXml

-- | Fetches all the Characters from EVE's XML API
getCharacters :: IO [Character]
getCharacters = do
  eveCharacterXML <- getCharacterXML
  return $ xmlToCharacters eveCharacterXML

getCharacterXML :: IO Text
getCharacterXML = getEveAPIRequest "/account/Characters.xml.aspx" []

getCalendarXML :: Int -> IO Text
getCalendarXML charID = getEveAPIRequest "/char/UpcomingCalendarEvents.xml.aspx" [("characterID", pack (show charID))] 

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