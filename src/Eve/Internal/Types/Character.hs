module Eve.Internal.Types.Character (Character(..))
where 

import Data.Text                    (Text)

-- | 'Character' represents the Character data from EVE's XML API.
data Character = Character
  { name :: Text
  , characterID :: Int
  , corporationName :: Text
  , corporationID :: Int
  , allianceName :: Text
  , allianceID :: Int
  , factionName :: Text
  , factionID :: Int
  }