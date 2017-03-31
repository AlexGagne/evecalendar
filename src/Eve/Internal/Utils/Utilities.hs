{-|
Module      : Eve.Internal.Utils.Utilities
Description : The module contains utility functions that could be used anywhere in the library.
Copyright   : (c) Alex Gagné, 2017
License     : MIT
Stability   : experimental

The module contains utility functions that could be used anywhere in the library.
-}

module Eve.Internal.Utils.Utilities (fromJust', textToUTCTime)
where 

import Data.Maybe       (fromMaybe)
import Data.Text        (Text, unpack)
import Data.Time        (UTCTime)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)

-- | 'fromJust'' will extract the value of a maybe or display the error passed in as a String
fromJust' :: Maybe a -> String -> a
fromJust' m err = fromMaybe (error err) m

-- | 'textToUTCTime' will transform a Text into UTCTime
textToUTCTime :: Text -> UTCTime
textToUTCTime dateString = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (unpack dateString) :: UTCTime