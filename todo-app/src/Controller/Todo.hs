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
import OpenTelemetry.Trace hiding (inSpan, inSpan', inSpan'')
import OpenTelemetry.Trace.Monad
import Data.Text
import qualified Utils.Transformer as TF
-- import qualified Utils.Utils as U
-- import qualified Exception as Exp
-- import qualified Control.Exception as CE

createTask :: SA.CreateTodoRequest -> F.Flow (SA.CreateTodoResponse)
createTask req@SA.CreateTodoRequest {task,description} =
  inSpan "createTask" defaultSpanArguments $ do
    now <- liftIO $ DateTime.getCurrentTimeIST
    id <- liftIO $ UUID.toText <$> UUID.nextRandom
    let status = "PENDING"
        respBody = SA.CreateTodoResponse id task description status True now
    _ <- QT.createTask req
    kvInsert <- KVQ.setExKey task respBody
    dummyCall <- liftIO $ Dummy.sendAck
    return respBody


fetchAllTask :: F.Flow (SA.FetchAllTaskResponse)
fetchAllTask = 
  inSpan "fetchAllTask" defaultSpanArguments $ do
    respFromDB <- QT.fetchAlltask
    let resp = TF.mkFetchAllTaskResponse respFromDB
    return resp
    

updateTask :: Text -> SA.UpdateTodoRequest -> F.Flow (Text)
updateTask taskId req@SA.UpdateTodoRequest{task} = 
  inSpan "updateTask" defaultSpanArguments $ do
    respFromDB <- QT.updateTask taskId req 
    deletekeyInRedis <-  KVQ.deleteKey task
    -- let resp = TF.mkUpdateTaskResponse respFromDB
        -- addKeyInRedis <- KVQ.setExKey task resp
    return $ "Task Updated"