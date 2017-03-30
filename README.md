
# EveCalendar
An Haskell app to download calendar data from EVE Online and upload it to Google Calendar, but also leave room for expansion if I ever want to leverage more of Eve Online's API. The src folder contains the EVE Online library that I may eventually flesh out and the app library will contain the server to send to Google Calendar.

## Building on a local machine

To build, you need to install stack. You may have to do some setup with stack before building.

1. Fork and clone
2. Enter this in a console:

``` bash
stack build
stack exec evecalendar
```

Those commands will update stack and download any missing libraries. Then, it will execute the server on port 8000. You need to add your eve online key id in your environment variables as KEY_ID and the verification code as V_CODE. A better way to pass in KEY_ID and V_CODE will eventually be implemented.

## Currently available
- The calendar data from EVE Online is fetched and the XML is parsed into a Haskell data format. The cache timer is included in the Haskell data. It is up to the application to handle and cache the data.

## TODO
- Implement unit tests (I'm having a hard time unit testing the IO functions, help would be appreciated)
- API Key and Verification Code not in an environment variable?
- Export the data to Google Calendar
- Explore new endpoints for EVE Online too see which would be interesting to use.
