module Eve.Internal.Utils.Utilities (fromJust', textToUTCTime)
where 

import Data.Maybe       (fromMaybe)
import Data.Text        (Text, unpack)
import Data.Time        (UTCTime)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)

-- Will throw error err if it is Nothing
fromJust':: Maybe a -> String -> a
fromJust' m err = fromMaybe (error err) m

textToUTCTime :: Text -> UTCTime
textToUTCTime dateString = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (unpack dateString) :: UTCTime