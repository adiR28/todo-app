module Storage.Types.API where

import Data.Text
import Data.Aeson
import Data.Time
import Servant.API.Generic (Generic)

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
      active :: Bool,
      updatedAt :: LocalTime
    }
    deriving (Generic,Show,ToJSON,FromJSON) 

data UpdateTodoRequest =
  UpdateTodoRequest
    {
      task :: Text,
      description :: Maybe Text,
      active ::  Bool,
      status ::  Text
    }
  deriving (Show,Generic,ToJSON,FromJSON) 

data UpdateTodoResponse =
  UpdateTodoResponse
  {
    task :: Text,
    description :: Maybe Text,
    status ::  Text,
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

data FetchDetailsResponse =
  FetchDetailsResponse
    {
      task :: Text,
      description :: Maybe Text,
      status :: Text,
      active :: Bool
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