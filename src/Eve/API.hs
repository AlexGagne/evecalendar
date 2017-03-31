{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Eve.API
Description : The module containing all the function calls to get Eve's XML Api
Copyright   : (c) Alex Gagné, 2017
License     : MIT
Stability   : experimental

The module containing all the function calls to get EVE's XML Api.
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
import Data.Functor.Identity

import Eve.Types                    (CalendarEvents, Credentials(..), Characters, Character, characterID)
import Eve.Internal.Utils.XmlReader (xmlToCalendarEvents, xmlToCharacters)

-- | Fetches all the calendar events from EVE's XML API
getUpcomingCalendarEvents :: Credentials -> Character -> IO CalendarEvents
getUpcomingCalendarEvents credentials character = do
  calendarEventXml <- getCalendarXML credentials $ characterID character
  return $ xmlToCalendarEvents calendarEventXml

-- | Fetches all the Characters from EVE's XML API
getCharacters :: Credentials -> IO Characters
getCharacters credentials = do
  eveCharacterXML <- getCharacterXML credentials
  return $ xmlToCharacters eveCharacterXML

getCharacterXML :: Credentials -> IO Text
getCharacterXML credentials = getEveAPIRequest credentials "/account/Characters.xml.aspx" []

getCalendarXML :: Credentials -> Int -> IO Text
getCalendarXML credentials charID = getEveAPIRequest credentials "/char/UpcomingCalendarEvents.xml.aspx" [("characterID", pack (show charID))] 

getEveAPIRequest :: Credentials -> Text ->  [(Text, Text)] -> IO Text
getEveAPIRequest credentials path options = do
  let authOptions = defaults {params = [("keyId", keyID), ("vCode", verifCode)] ++ options} 
  response <- getWith authOptions (eveBaseUrl ++ unpack path)
  return $ decodeUtf8 . toStrict $ response ^. responseBody
  where 
    eveBaseUrl = "https://api.eveonline.com"
    keyID = keyId credentials
    verifCode = vCode credentials