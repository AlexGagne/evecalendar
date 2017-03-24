module Eve.Utils.Utilities (fromJust')
where 

import Data.Maybe (fromMaybe)

-- Will throw error err if it is Nothing
fromJust':: Maybe a -> String -> a
fromJust' m err = fromMaybe (error err) m