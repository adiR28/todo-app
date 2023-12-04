{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module App.Server where

import qualified Routes.Routes as R
import Servant
import Storage.Types.App
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified Exception as Exp
import qualified Control.Exception as CE
import Utils.Jsonifier  as UJ
import qualified Network.HTTP.Media as M
import qualified Data.List.NonEmpty as NE
import qualified Jsonifier as J
import qualified Data.ByteString.Lazy as BSL


type FlowServer = 
  ServerT R.APIs (ReaderT Env (ExceptT ServerError IO))

todoProxy :: Proxy R.APIs
todoProxy = Proxy

todoServer :: Env -> Server R.APIs
todoServer env = hoistServer todoProxy (f env ) todoServers'
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env' r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env'
      case eResult  of
        Left err -> CE.throw $ Exp.Exception $ "Unable to Start Server :" <> show err
        Right res -> pure res

todoServers' :: FlowServer        
todoServers' = todoAPI :<|> userAPI :<|> applicationAPI

todoAPI = R.createTodo :<|> R.updateTodo :<|> R.fetchAllTodo :<|> R.getDetailsTodo :<|> R.loadTest

userAPI = R.createUser :<|> R.updateUser

applicationAPI = R.appCheck
