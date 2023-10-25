module Storage.Types.API where

import Data.Text hiding(map)
import Data.Aeson
import Data.Time
import Servant.API.Generic (Generic)
import qualified Jsonifier as J
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Bifunctor as Bi
import qualified Data.Text as T
-- import qualified Data.Time.Zones.Internal as DTZI

-- User

data CreateUserRequest =
  CreateUserRequest
    {
      name :: Text,
      email :: Maybe Text
    }
  deriving (Generic,Show,ToJSON,FromJSON)

data CreateUserResponse =
  CreateUserResponse
    {
      id :: Text,
      name :: Text,
      email :: Maybe Text,
      createdAt :: LocalTime
    }
  deriving (Generic,Show,ToJSON,FromJSON)

data UpdateUserResponse =
  UdpateUserResponse
    {
      id :: Text,
      name :: Text,
      email :: Maybe Text,
      active :: Bool,
      updatedAt :: LocalTime
    }
  deriving (Generic,Show,ToJSON,FromJSON)

-- Todo

data CreateTodoRequest =
  CreateTodoRequest
    {
     task :: Text,
     description :: Maybe Text
    }
  deriving (Generic,Show,ToJSON,FromJSON)

data CreateTodoResponse =
  CreateTodoResponse
    {
      id :: Text,
      task :: Text,
      description :: Maybe Text,
      status :: Text,
      createdAt :: LocalTime
    }
    deriving (Generic,Show,ToJSON,FromJSON)

data UpdateTodoRequest =
  UpdateTodoRequest
    {
      task :: Text,
      description :: Maybe Text,
      status ::  Maybe Text,
      active :: Maybe Bool
    }
  deriving (Show,Generic,ToJSON,FromJSON)

data UpdateTodoResponse =
  UpdateTodoResponse
  {
    task :: Text,
    description :: Maybe Text,
    status :: Maybe Text,
    active :: Bool,
    updatedAt :: LocalTime
  }
  deriving (Show,Generic,ToJSON,FromJSON)

data FetchAllTaskResponse =
  FetchAllTaskResponse
    {
      tasks :: [(Text,Text)]
    }
  deriving (Show,Generic,ToJSON,FromJSON)

data FetchDetailsResponse =
  FetchDetailsResponse
    {
      task :: Text,
      description :: Text,
      status :: Text,
      active :: Bool
    }
  deriving (Show,Generic,ToJSON,FromJSON)





data Status = SUCCESS | FAILED | DEEMED 
  deriving(Show,Generic,ToJSON,FromJSON)

data JsonifierExp =
  JsonifierExp
    {
      task :: Text,
      active :: Bool,
      description :: Maybe Text,
      udf :: Value,
      createdAt :: LocalTime,
      status :: Status
    } deriving (Show,Generic,ToJSON,FromJSON)


statusToJson :: Status -> J.Json
statusToJson status = J.textString $ T.pack $ show status

jsonifierExp :: JsonifierExp -> J.Json
jsonifierExp j = buildMaybeValueToJson $ Just $ Aeson.toJSON j
  -- J.object
  --   [
  --       ("task",J.textString task)
  --     , ("active" ,J.bool active) 
  --     , ("description" , buildMaybeTextToJSON description)
  --     , ("udf",valueToJson udf)
  --     , ( "status",statusToJson status)
  --   ]


valueToJson :: Aeson.Value -> J.Json
valueToJson Aeson.Null = J.null
valueToJson (Aeson.Array arr) = J.array $ valueToJson <$> arr
valueToJson (Aeson.String txt) = J.textString txt
valueToJson (Aeson.Number num) = J.scientificNumber num
valueToJson (Aeson.Bool bool) = J.bool bool
valueToJson (Aeson.Object obj) = J.object $ Bi.second valueToJson <$> HashMap.toList obj


buildMaybeValueToJson :: Maybe Aeson.Value -> J.Json
buildMaybeValueToJson Nothing = J.null
buildMaybeValueToJson (Just val) = valueToJson val

-- buildMaybeTextToJSON :: Maybe Text -> J.Json 
-- buildMaybeTextToJSON Nothing = J.null
-- buildMaybeTextToJSON (Just txt) = J.textString txt

-- localTimeToJson :: LocalTime -> J.Json
-- localTimeToJson LocalTime{..} = 
--   J.object 
--     [
--         ("localDay", J.textString $ T.pack $ show localDay)
--       , ("localTimeOfDay", timeOfDay localTimeOfDay )  
--     ]


-- timeOfDay :: TimeOfDay -> J.Json
-- timeOfDay TimeOfDay{..} = 
--   J.object
--     [
--         ("todHour",J.intNumber todHour)
--       , ("todMin" , J.intNumber todMin)
--       , ("todSec", J.scientificNumber $ fromInteger $ DTZI.picoToInteger todSec)  -- in Pico Seconds , converted to Scientific. range is 0-61 only 
--     ]