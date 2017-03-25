module Main where

import Eve.API (getUpcomingCalendarEvents)

main :: IO ()
main = do
  calendarEvents <- getUpcomingCalendarEvents
  print calendarEvents
