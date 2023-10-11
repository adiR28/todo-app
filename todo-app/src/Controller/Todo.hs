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

createTask :: SA.CreateTodoRequest -> F.Flow (SA.CreateTodoResponse)
createTask req@SA.CreateTodoRequest {task,description} = do
  inSpan "createTask" defaultSpanArguments $ do
    now <- liftIO $ DateTime.getCurrentTimeIST
    id <- liftIO $ UUID.toText <$> UUID.nextRandom
    let status = "PENDING"
        respBody = SA.CreateTodoResponse id task description status now
    _ <- QT.createTask req
    kvInsert <- KVQ.setExKey task $ SA.CreateTodoResponse id task description status now
    dummyCall <- liftIO $ Dummy.sendAck
    return respBody
