{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
import Utils.Jsonifier  as UJ
import qualified Network.HTTP.Media as M
import qualified Data.List.NonEmpty as NE
import qualified Jsonifier as J
import qualified Data.ByteString.Lazy as BSL
import Data.Data (Typeable)
import Data.Aeson
import qualified Data.Text as DT

data JSONIFIER deriving Typeable
instance Accept JSONIFIER where
  contentTypes _ =
    "application" M.// "jsonifier" M./: ("charset", "utf-8") NE.:|
    [ "application" M.// "jsonifier" ]

instance {-# OVERLAPPABLE #-} UJ.Jsonifier a => MimeRender JSONIFIER a where
  mimeRender _ val = BSL.fromStrict $ J.toByteString $ UJ.toJsonifier val

-- instance FromJSON a => MimeUnrender JSONIFIER a where
--     mimeUnrender _ = eitherDecodeLenient


type TodoAPIs = "todo" :>
  ("create" :> ReqBody '[JSON] SA.CreateTodoRequest :> Post '[JSONIFIER] (SA.CreateTodoResponse)
    :<|> "update" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "fetchAll" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "getDetails" :> Capture "taskName" String :> Get '[JSON] String
    :<|> "loadTest" :> Get '[JSON] DT.Text )
 
type ApplicationAPIs = "application" :> "app" :> Get '[JSON,PlainText] String

type UserAPIs = "user" :>
  ("create" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "update" :> ReqBody '[JSON] String :> Post '[JSON] String)
   
type APIs = TodoAPIs  :<|> UserAPIs :<|> ApplicationAPIs

-- TODO

createTodo :: SA.CreateTodoRequest  -> FlowHandler (SA.CreateTodoResponse)
createTodo req =  do
  liftIO $ putStrLn  $ "createTODO API called " <> show req
  flowHandlerWithEnv(CT.createTask req)

updateTodo :: String -> FlowHandler String
updateTodo = return 

fetchAllTodo :: String -> FlowHandler String
fetchAllTodo = return

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

loadTest :: FlowHandler DT.Text
loadTest = flowHandlerWithEnv(CT.loadTest)


flowHandlerWithEnv :: F.Flow a -> FlowHandler a
flowHandlerWithEnv flowFunc = do
  env@Env{..} <- ask
  lift $ ExceptT $ try $ runReaderT flowFunc env