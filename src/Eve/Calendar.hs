{-# LANGUAGE DisambiguateRecordFields #-}

module Eve.Calendar
    ( startServer
    ) where

import           Control.Monad                  (msum)
import           Data.Text                      (unpack)
import           Happstack.Server               (ok, dirs, nullConf, seeOther)
import qualified Happstack.Server                as Happ
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit as I

import Eve.API                                    (getUpcomingCalendarEvents)

startServer = do
  calendarEvents <- getUpcomingCalendarEvents
  print calendarEvents
  {-

startServer :: IO ()
startServer =  do
  config <- I.cmdArgs aConfig
  Happ.simpleHTTP (hConf config) routes

routes :: Happ.ServerPartT IO String
routes = msum [ dirs "evecalendar" $ transferEveCalendar]

transferEveCalendar :: Happ.ServerPartT IO String
transferEveCalendar = ok "hello"



-- Config
--------------------------------------------------------------------------------


data Config =
  Config { port :: Int, timeout :: Int } deriving ( Show, Eq, Data, Typeable )

hConf :: Config -> Happ.Conf
hConf Config {..} = Happ.nullConf { Happ.timeout = timeout, Happ.port = port }

aConfig :: Config
aConfig =
  Config { port    = 8000  &= I.help "Port number"
                           &= I.typ "INT"
         , timeout = 30    &= I.help "Timeout"
                           &= I.typ "SECONDS"
         }
    &= I.summary "EveCalendar server"
    &= I.program "server"
    -}
