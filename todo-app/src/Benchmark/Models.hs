module Benchmark.Models where

import Prelude hiding (id)
import Data.Text
import Data.Aeson hiding (Result)
import Servant.API.Generic (Generic)
import qualified Utils.UtilsTH as UTH
import Data.Time
import Utils.Jsonifier
import Data.Scientific

data Metadata = Metadata
  { result_type :: Text
  }
  deriving (Eq, Show,Generic,ToJSON,FromJSON)

-- instance NFData Metadata

data Geo = Geo
  { type_ :: Text,
    coordinates :: Scientific
  }
  deriving (Eq, Show,Generic,ToJSON,FromJSON)

-- instance NFData Geo

data Story = Story
  { from_user_id_str :: Text,
    profile_image_url :: Text,
    created_at :: Text,
    from_user :: Text,
    id_str :: Text,
    metadata :: Metadata,
    to_user_id :: Maybe Int,
    text :: Text,
    id :: Int,
    from_user_id :: Int,
    geo :: Maybe Geo,
    iso_language_code :: Text,
    to_user_id_str :: Maybe Text,
    source :: Text
  }
  deriving (Eq, Show,Generic,ToJSON,FromJSON)

data Result = Result
  { results :: [Story],
    max_id :: Int,
    since_id :: Int,
    refresh_url :: Text,
    next_page :: Text,
    results_per_page :: Int,
    page :: Int,
    completed_in :: Scientific,
    since_id_str :: Text,
    max_id_str :: Text,
    query :: Text
  }
  deriving (Eq, Show,Generic,ToJSON,FromJSON)


$(UTH.mkJsonifierInstanceTH ''Day)
$(UTH.mkJsonifierInstanceTH ''TimeOfDay)
$(UTH.mkJsonifierInstanceTH ''LocalTime)
$(UTH.mkJsonifierInstanceTH ''Metadata)
$(UTH.mkJsonifierInstanceTH ''Geo)
$(UTH.mkJsonifierInstanceTH ''Story)
$(UTH.mkJsonifierInstanceTH ''Result)

