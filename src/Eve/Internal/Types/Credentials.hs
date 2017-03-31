{-|
Module      : Eve.Internal.Types.Credentials
Description : The module contains the data and related functions to the Credentials
Copyright   : (c) Alex Gagné, 2017
License     : MIT
Stability   : experimental
-}

module Eve.Internal.Types.Credentials (Credentials(..))
where 

import Data.Text

-- | 'Credentials' represents the keyId and verification code needed to log into EVE's XML API
data Credentials = Credentials
  { keyId :: Text
  , vCode :: Text
  }