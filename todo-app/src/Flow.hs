module Flow where

import Control.Monad.Trans.Reader
import Prelude
import Storage.Types.App
import qualified Config.Types as Conf
import qualified Control.Concurrent as Concurrent

type Flow = ReaderT Env IO

data Awaitable s = Awaitable (Concurrent.MVar s)

getConfig :: Flow Conf.Config
getConfig = do
  Env {..} <- ask
  return config