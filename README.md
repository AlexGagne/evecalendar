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

This will build and execute the server. By default, it will listen on port 8000.

## Launch on Heroku

You need to have the heroku toolbelt installed

1. Fork and clone
2. Enter this in a console:

`
heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git
git push heroku master
`

This will create a new website at a new random address generated by Heroku and push the project to it.


## Currently available

A nice endpoint using the Happstack framework.

## TODO

- Import XML data from EVE Online's API
- Format the data to be accepted by Google Calendar
- Export the data to Google Calendar
- Explore new endpoints for EVE Online too see which would be interesting to use.
