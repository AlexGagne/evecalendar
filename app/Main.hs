module Main where

import Eve.API (getCharacters, getUpcomingCalendarEvents)
import Eve.Types (characters, cachedUntil)

main :: IO ()
main = do
  charactersFromAPI <- getCharacters
  calendarEvents <- getUpcomingCalendarEvents $ head $ characters charactersFromAPI
  print $ cachedUntil calendarEvents
