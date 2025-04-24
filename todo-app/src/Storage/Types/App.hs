module Storage.Types.App where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Servant
import qualified Config.Types as Conf
import qualified Control.Concurrent.Chan.Unagi.Bounded as UChan
import qualified Data.Monoid as DM
import qualified Control.Concurrent as Concurrent


type CallbackQueue a = (UChan.InChan (IO a ) , UChan.OutChan (IO  a))

data Env = 
  Env 
  {
      config :: Conf.Config
    , callBackQueue :: CallbackQueue DM.Any
  }

type FlowHandler = ReaderT Env (ExceptT ServerError IO)