module Routes.Routes where

import Servant
import qualified Storage.Types.API as SA
import qualified Flow as F
import qualified Controller.Todo as CT
import Storage.Types.App
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Exception
import Control.Monad.IO.Class
import Data.Text

type TodoAPIs = "todo" :>
  ("create" :> ReqBody '[JSON] SA.CreateTodoRequest :> Post '[JSON] (SA.CreateTodoResponse)
    :<|> "update" :> Capture "taskId" Text :> ReqBody '[JSON] SA.UpdateTodoRequest :> Post '[JSON,PlainText] (Text)
    :<|> "fetchAll" :> Get '[JSON] SA.FetchAllTaskResponse
    :<|> "getDetails" :> Capture "taskName" String :> Get '[JSON] String )
 
type ApplicationAPIs = "application" :> "app" :> Get '[JSON,PlainText] String

type UserAPIs = "user" :>
  ("create" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "update" :> ReqBody '[JSON] String :> Post '[JSON] String)
   
type APIs = TodoAPIs  :<|> UserAPIs :<|> ApplicationAPIs

-- TODO

createTodo :: SA.CreateTodoRequest  -> FlowHandler (SA.CreateTodoResponse)
createTodo req =  do
  liftIO $ putStrLn  $ "createTask API called " <> show req
  flowHandlerWithEnv(CT.createTask req)

updateTodo :: Text -> SA.UpdateTodoRequest -> FlowHandler (Text)
updateTodo taskId req= do
  liftIO $ putStrLn  $ "updateTask API called " <> show req
  flowHandlerWithEnv(CT.updateTask taskId req)

fetchAllTodo :: FlowHandler (SA.FetchAllTaskResponse)
fetchAllTodo = do
  liftIO $ putStrLn "fetchAllTask API called "
  flowHandlerWithEnv(CT.fetchAllTask)

getDetailsTodo :: String ->  FlowHandler String
getDetailsTodo = return

-- APPLICATION
appCheck :: FlowHandler String
appCheck =  return "Todo App is Running"

-- USER

createUser :: String -> FlowHandler String
createUser = return

updateUser :: String -> FlowHandler String
updateUser = return

flowHandlerWithEnv :: F.Flow a -> FlowHandler a
flowHandlerWithEnv flowFunc = do
  env@Env{..} <- ask
  lift $ ExceptT $ try $ runReaderT flowFunc env