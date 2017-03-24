module Eve.Types.Character (Character)
where 

import Data.Text (Text)

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