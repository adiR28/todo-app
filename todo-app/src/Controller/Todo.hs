module Controller.Todo where

import qualified Storage.Types.API as SA
import qualified Flow as F
import qualified Storage.KV.Queries as KVQ
import qualified Storage.DB.Queries.Todo as QT
import Control.Monad.IO.Class
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Utils.DateTime as DateTime
import qualified External.DummyServer as Dummy
import qualified Data.Aeson as Aeson
import qualified Utils.Jsonifier as UJ
import qualified Utils.Transformer as TF
-- import qualified Jsonifier as J
-- import Data.ByteString.UTF8 as DSU


fetchAllTask :: F.Flow (SA.FetchAllTaskResponse)
fetchAllTask = do
  -- inSpan "fetchAllTask" defaultSpanArguments $ do
    respFromDB <- QT.fetchAlltask
    let temp = Aeson.toJSON $ head respFromDB
    _ <- liftIO $ putStrLn $ " DB resp " <> show temp
    let resp = TF.mkFetchAllTaskResponse respFromDB
    return resp


createTask :: SA.CreateTodoRequest -> F.Flow (SA.CreateTodoResponse)
createTask req@SA.CreateTodoRequest {task,description} = do
  now <- liftIO $ DateTime.getCurrentTimeIST
  id <- liftIO $ UUID.toText <$> UUID.nextRandom
  let status' = "PENDING"
      respBody = SA.CreateTodoResponse id task description status' now (Left "abc")
  _ <- QT.createTask req
  fetch <- fetchAllTask
  -- let testing = mkDummyJsonifierExp
  -- let ctx = SA.ABC "hello" ["Aditya","Ranjan"]
  -- let status = SA.SUCCESS
  let toJSONValtesting = Aeson.toJSON respBody
  let toJSONFierValtesting = UJ.toJsonifier respBody
  _ <- liftIO $ putStrLn $ "toJSONtesting value : " <> show toJSONValtesting
  _ <- liftIO $ putStrLn $ "toJSONFiertesting value : " <> show toJSONFierValtesting
  _ <- liftIO $ putStrLn $ "---------------------------------"
  -- let toJSONValStatus = Aeson.toJSON status
  -- let toJSONFierValStatus = UJ.toJsonifier status
  -- _ <- liftIO $ putStrLn $ "toJSONStatus value : " <> show toJSONValStatus
  -- _ <- liftIO $ putStrLn $ "toJSONFierStatus value : " <> show toJSONFierValStatus
  -- _ <- liftIO $ putStrLn $ "---------------------------------"
  -- let toJSONValCTX = Aeson.toJSON ctx
  -- let toJSONFierValCTX = UJ.toJsonifier ctx
  -- _ <- liftIO $ putStrLn $ "toJSONValCTX value : " <> show toJSONValCTX
  -- _ <- liftIO $ putStrLn $ "toJSONFierValCTX value : " <> show toJSONFierValCTX
  kvInsert <- KVQ.setExKey task $ SA.CreateTodoResponse id task description status' now (Right 1)
  dummyCall <- liftIO $ Dummy.sendAck

  return respBody

-- updateTask

-- deleteTask


mkDummyJsonifierExp :: SA.JsonifierExp
mkDummyJsonifierExp = SA.JsonifierExp "aditya" True (Just "Tetsing") (Aeson.String "value") "now" ["hello"] Nothing Nothing []