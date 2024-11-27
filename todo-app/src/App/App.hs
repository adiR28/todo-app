module App.App where

import Servant
import qualified App.Server as S
import Network.Wai.Handler.Warp
import Storage.Types.App
import qualified Database.Redis as Redis
import qualified Data.Text.Encoding as DTE
import qualified Config.Types  as Conf
import qualified Config.Config as CC
import qualified  Middleware.Middleware as Middleware
import qualified Controller.UChan as Chan

app :: Env -> Application
app = serve S.todoProxy . S.todoServer

runServer :: IO ()
runServer = do
  conf <- CC.config
  (chan, _) <- Chan.initiateChan
  let env = Env conf chan
  getKvConnection <- prepareKVConnection (Conf.kvConfig conf) (Conf.isRedisClusterEnabled conf)
  putStrLn $ "APP is Running :" <> show (Conf.port conf)
  run (Conf.port conf) $ Middleware.customMiddleware $ app env 

prepareKVConnection :: Redis.ConnectInfo -> Bool -> IO ()
prepareKVConnection redisConf isClusterEnabled = do
  conn <- if  isClusterEnabled then Redis.connectCluster redisConf else Redis.connect redisConf
  Redis.runRedis conn $ Redis.set (DTE.encodeUtf8 "dummy_testing") "dummy"
  return ()

