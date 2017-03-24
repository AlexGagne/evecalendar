
# EveCalendar
An Haskell app to download calendar data from EVE Online and upload it to Google Calendar, but also leave room for expansion if I ever want to leverage more of Eve Online's API.

## Building on a local machine

To build, you need to install stack. You may have to do some setup with stack before building.

1. Fork and clone
2. Enter this in a console:

`
stack build
stack exec evecalendar
`
Those commands will update stack and download any missing libraries. Then, it will execute the server on port 8000.

## Launch on Heroku

TBD

## Currently available

- The calendar data from EVE Online is fetched and the XML is parsed into a Haskell data format.

## TODO
- Minor refactoring (more code reuse, separate xml into another file, only include what is necessary)
- Implement unit tests.
- Deal with cache timers
- Export the data to Google Calendar
- Explore new endpoints for EVE Online too see which would be interesting to use.
