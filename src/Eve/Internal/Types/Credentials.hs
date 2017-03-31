module Eve.Internal.Types.Credentials (Credentials(..))
where 

import Data.Text

data Credentials = Credentials
  { keyId :: Text
  , vCode :: Text
  }