
# EveCalendar
An Haskell app to download calendar data from EVE Online and upload it to Google Calendar, but also leave room for expansion if I ever want to leverage more of Eve Online's API. The src folder contains the EVE Online library that fetches from EVE Online and the app folder contains the application that will pull data and send it to Google Calendar.

[![Travis](https://img.shields.io/travis/alexgagne/evecalendar.svg)]()
[![license](https://img.shields.io/github/license/alexgagne/evecalendar.svg)]()

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
The calendar data from EVE Online is fetched and the XML is parsed into a Haskell data format. The cache timer is included in the Haskell data. It is up to the application to handle and cache the data. The app included does not currently cache the data.

## State of the library
 The library used to fetch EVE Online's data is still experimental. It is currently not automatically tested. It also only contains the functions to fetch from the Characters endpoint and the UpcomingCalendarEvents endpoint of EVE's XML API. It would be relatively easy to expand the library to contain more endpoints and I may do so in the future.
