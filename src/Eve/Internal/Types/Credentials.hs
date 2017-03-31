module Eve.Internal.Types.Credentials (Credentials(..))
where 

import Data.Text

-- | 'Credentials' represents the keyId and verification code needed to log into EVE's XML API
data Credentials = Credentials
  { keyId :: Text
  , vCode :: Text
  }