{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module EveCalendar
    ( startServer
    ) where

import           Control.Monad                  (msum)
import           Data.Data
import           Happstack.Server               (ok, dirs, nullConf, seeOther)
import qualified Happstack.Server                as Happ
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit as I

startServer :: IO ()
startServer =  do
  config <- I.cmdArgs aConfig
  Happ.simpleHTTP (hConf config) routes

routes :: Happ.ServerPartT IO String
routes = msum [ dirs "evecalendar" $ transferEveCalendar]

transferEveCalendar :: Happ.ServerPartT IO String
transferEveCalendar = ok "Done transfering Eve Online calendar to Google Calendar"


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
