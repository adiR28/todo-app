-- {-# LANGUAGE  DuplicateRecordFields  #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}

module Storage.Types.API where

import Data.Text hiding(map)
import Data.Aeson
import Data.Time
import Servant.API.Generic (Generic)
import qualified Jsonifier as J
-- import qualified Data.Aeson as Aeson
-- import qualified Data.HashMap.Strict as HashMap
-- import qualified Data.Bifunctor as Bi
import qualified Data.Text as T
import qualified Utils.UtilsTH as UTH
import Data.Functor.Identity (Identity)
import Data.Scientific (Scientific)
import qualified Data.Bifunctor as Bi
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Time.Zones.Internal as DTZI
import Prelude
import Data.Fixed ( Pico )
import qualified Data.Aeson.TH as AesonTH
import qualified Data.Aeson as Aeson
import Utils.Jsonifier
import qualified Utils.Jsonifier as UJ
import Data.Either

-- User

-- class Jsonifier a where
--   toJsonifier :: a -> J.Json

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
      createdAt :: LocalTime,
      temp :: Either Text Int
    }
    deriving (Generic,Show,ToJSON,FromJSON)

$(UTH.mkJsonifierInstanceTH ''CreateTodoResponse)

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
      tasks :: [FetchResponsePayload]
    }
  deriving (Show,Generic,ToJSON,FromJSON) 


data FetchResponsePayload =
  FetchResponsePayload
    {
      id :: Text,
      taskName :: Text,
      taskDescription :: Maybe Text,
      status :: Text
    } deriving (Show,Generic,ToJSON,FromJSON)  

data FetchDetailsResponse =
  FetchDetailsResponse
    {
      task :: Text,
      description :: Text,
      status :: Text,
      active :: Bool
    }
  deriving (Show,Generic,ToJSON,FromJSON)

data Status  = SUCCESS | FAILURE | DEEMED
  deriving (Show,Generic,FromJSON,ToJSON)

-- statusJson :: Status -> J.Json
-- statusJson st = J.textString $ T.pack $ show st
-- data info : DatatypeInfo {datatypeContext = [], datatypeName = Storage.Types.API.Status, datatypeVars = [], datatypeInstTypes = [], datatypeVariant = Datatype, datatypeCons = [ConstructorInfo {constructorName = Storage.Types.API.SUCCESS, constructorVars = [], constructorContext = [], constructorFields = [], constructorStrictness = [], constructorVariant = NormalConstructor},ConstructorInfo {constructorName = Storage.Types.API.FAILURE, constructorVars = [], constructorContext = [], constructorFields = [], constructorStrictness = [], constructorVariant = NormalConstructor},ConstructorInfo {constructorName = Storage.Types.API.DEEMED, constructorVars = [], constructorContext = [], constructorFields = [], constructorStrictness = [], constructorVariant = NormalConstructor}]}
data JsonifierExp =
  JsonifierExp
    {
      task :: Text,
      active :: !Bool,
      description :: Maybe Text,
      udf :: !Value,
      createdAt :: Text,
      status :: [Text],
      abc :: Maybe Bool,
      localTime :: LocalTime,
      maybeLocalTime :: Maybe LocalTime,
      maybeList ::  Maybe [Text],
      temp :: [Maybe Text]
      eth :: Either Text Int

    } deriving (Show,Generic,FromJSON)

data CTX = ABC  Text [Text] | DEF [Int]
  deriving (Show,Generic,FromJSON,ToJSON)

-- $(AesonTH.deriveToJSON AesonTH.defaultOptions ''CTX)
-- $(AesonTH.deriveToJSON AesonTH.defaultOptions ''Status)
-- $(AesonTH.deriveToJSON AesonTH.defaultOptions ''JsonifierExp)

-- data info : DatatypeInfo {datatypeContext = [], datatypeName = Storage.Types.API.CTX, datatypeVars = [], datatypeInstTypes = [], datatypeVariant = Datatype, datatypeCons = [ConstructorInfo {constructorName = Storage.Types.API.ABC, constructorVars = [], constructorContext = [], constructorFields = [AppT ListT (ConT GHC.Base.String)], constructorStrictness = [FieldStrictness {fieldUnpackedness = UnspecifiedUnpackedness, fieldStrictness = UnspecifiedStrictness}], constructorVariant = NormalConstructor},ConstructorInfo {constructorName = Storage.Types.API.DEF, constructorVars = [], constructorContext = [], constructorFields = [AppT ListT (ConT GHC.Types.Int)], constructorStrictness = [FieldStrictness {fieldUnpackedness = UnspecifiedUnpackedness, fieldStrictness = UnspecifiedStrictness}], constructorVariant = NormalConstructor}]}

newtype Temp = Temp Int -- Normal Constructor

data Testing =
  Testing
    {
      name :: Text,
      value :: Text,
      temp2 :: Maybe Value,
      temp3 :: [Maybe Bool],
      temp4 :: [Scientific],
      eth :: Either Text Int

    }
-- $(AesonTH.deriveToJSON AesonTH.defaultOptions ''Testing)

-- instance (Jsonifier abc) => Jsonifier (Testing abc) where
--   toJsonifier (Testing a b c d e) = 
--     J.object 
--       [
--         ("text", toJsonifier a),
--         ("temp1", toJsonifier b),
--         ("temp2", toJsonifier c),
--         ("temp3", toJsonifier d),
--         ("temp4", toJsonifier e)
--       ]

data TodosT f =
  Todos
    {
      _id :: f ( Text),
      _task :: f Text,
      _description :: f ( Text),
      _status :: f [Text], -- TODO add enum
      _active :: f (Maybe Bool),
      _createdAt :: f LocalTime,
      _updatedAt :: f LocalTime,
      _udf :: f ( Value),
      temp :: f [Maybe Text]
    }deriving (Generic)

type Todo = TodosT Identity

-- $(AesonTH.deriveToJSON AesonTH.defaultOptions ''TodosT)

$(UTH.mkJsonifierInstanceTH ''Day)
$(UTH.mkJsonifierInstanceTH ''TimeOfDay)
$(UTH.mkJsonifierInstanceTH ''LocalTime)
$(UTH.mkJsonifierInstanceTH ''JsonifierExp)



-- RecordConstructor
$(UTH.mkJsonifierInstanceTH ''Status)
-- $(UTH.mkJsonifierInstanceTH ''CTX) -- NormalCosntructor
-- $(UTH.mkJsonifierInstanceTH ''TodosT)
-- $(UTH.mkJsonifierInstanceTH ''Testing)