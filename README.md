
# EveCalendar
An Haskell app to download calendar data from EVE Online and upload it to Google Calendar, but also leave room for expansion if I ever want to leverage more of Eve Online's API. The src folder contains the EVE Online library that I may eventually flesh out and the app folder contains the application that will pull data and send it to Google Calendar.

## Building on a local machine

To build, you need to install stack. You may have to do some setup with stack before building.

1. Fork and clone
2. Enter this in a console:

``` bash
stack build
stack exec evecalendar
```

Those commands will update stack and download any missing libraries. The app included will get the key and verification code from your environment variables. You need to add your EVE Online key id in your environment variables as KEY_ID and the verification code as V_CODE.

## Currently available
- The calendar data from EVE Online is fetched and the XML is parsed into a Haskell data format. The cache timer is included in the Haskell data. It is up to the application to handle and cache the data. The app included does not currently cache the data.
