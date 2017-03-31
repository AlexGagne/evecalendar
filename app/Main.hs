module Main where

import Control.Monad.IO.Class          (liftIO)
import Data.Text                       (Text, pack)
import qualified System.Environment as E

import Eve.API                         (getCharacters, getUpcomingCalendarEvents)
import Eve.Types                       (characters, cachedUntil, Credentials(..))


main :: IO ()
main = do
  credentials <- cred
  charactersFromAPI <- getCharacters credentials
  calendarEvents <- getUpcomingCalendarEvents credentials $ head $ characters charactersFromAPI
  print $ cachedUntil calendarEvents
  where
    cred = do 
      keyId <- getKeyId
      vCode <- getVCode
      return $ Credentials keyId vCode

getKeyId :: IO Text
getKeyId = do
  keyID <- E.getEnv "KEY_ID"
  return $ pack keyID

getVCode :: IO Text
getVCode = do
  vCode <- E.getEnv "V_CODE"
  return $ pack vCode
