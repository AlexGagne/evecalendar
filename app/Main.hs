module Main where

import Eve.API (getCharacters, getUpcomingCalendarEvents)
import Eve.Types (characters)

main :: IO ()
main = do
  charactersFromAPI <- getCharacters
  calendarEvents <- getUpcomingCalendarEvents $ head $ characters charactersFromAPI
  print calendarEvents
