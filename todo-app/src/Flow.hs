module Flow where

import Control.Monad.Trans.Reader
import Prelude
import Storage.Types.App
import qualified Config.Types as Conf
import OpenTelemetry.Trace.Monad as OTM
import qualified OpenTelemetry.Trace.Core as OTC

type Flow = ReaderT Env IO

instance OTM.MonadTracer Flow where
  getTracer = do
    tp <- OTC.getGlobalTracerProvider
    return $ OTC.makeTracer tp "tykeagent-application" OTC.tracerOptions


getConfig :: Flow Conf.Config
getConfig = do
  Env {..} <- ask
  return config