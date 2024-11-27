{-# LANGUAGE TypeApplications #-}
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
import qualified Data.Text as DT
import qualified System.Time.Extra as STE
import qualified Control.Concurrent as Concurrent
import qualified Data.Foldable as DF
import qualified Control.DeepSeq as CD
import qualified Control.Monad as CM
import qualified Control.Exception as CE
import qualified Control.Concurrent.Chan.Unagi.Bounded as UChan
import qualified Storage.Types.App as TA
import qualified Control.Monad.Trans.Reader as MTR
import Data.Monoid
import qualified Unsafe.Coerce as Unsafe
-------------------------------------------- Thread Functionality -------------------------------------------
numberofThreads :: Int
numberofThreads = 2

forkFlow :: DT.Text -> (IO a) -> IO (F.Awaitable (Either DT.Text a))
forkFlow description flow = do
  awaitableMVar <- Concurrent.newEmptyMVar
  Concurrent.forkIO $ do
    (eitherResp :: Either CE.SomeException a) <- CE.try flow
    case eitherResp of
      Left err -> Concurrent.putMVar awaitableMVar $ (Left $ DT.pack $ show err) 
      Right resp -> Concurrent.putMVar awaitableMVar $ Right resp
  return $ F.Awaitable awaitableMVar

createTask :: SA.CreateTodoRequest -> F.Flow (SA.CreateTodoResponse)
createTask req@SA.CreateTodoRequest {task,description} = do
  now <- liftIO $ DateTime.getCurrentTimeIST
  id <- liftIO $ UUID.toText <$> UUID.nextRandom
  let status = "PENDING"
      respBody = SA.CreateTodoResponse id task description status now
  _ <- QT.createTask req
  CM.void $ liftIO $ STE.sleep 0.03
  kvInsert <- KVQ.setExKey task $ SA.CreateTodoResponse id task description status now
  dummyCall <- liftIO $ Dummy.sendAck
  forkThreadHelper numberofThreads id
  return respBody

forkThreadHelper :: Int -> DT.Text -> F.Flow()
forkThreadHelper threadCount parentThreadId =
  liftIO $ mapM_ (\x -> forkFlow "forkFlow Called" $ threadCallerFunction ("parentThreadId : " <> parentThreadId <> " threadNumber : " <> (DT.pack $ show x) )
    ) [1..threadCount]

threadCallerFunction :: DT.Text -> IO Int
threadCallerFunction threadMsg = do
  putStrLn $ "forking thread : " <> (DT.unpack threadMsg)
  let r1 = CD.force $ (DF.foldl' (\acc x -> x + acc) 0 [1..999999] :: Integer)
      _ = CD.force $ (DF.foldl'(\acc x -> x + acc) 0 [1..999999] :: Integer)
      _ = CD.force $ (DF.foldl'(\acc x -> x + acc) 0 [1..999999] :: Integer)
      _ = CD.force $ (DF.foldl'(\acc x -> x + acc) 0 [1..999999] :: Integer)
      _ = CD.force $ (DF.foldl'(\acc x -> x + acc) 0 [1..999999] :: Integer)
      _ = CD.force $ (DF.foldl'(\acc x -> x + acc) 0 [1..999999] :: Integer)
      _ = CD.force $ (DF.foldl'(\acc x -> x + acc) 0 [1..999999] :: Integer)
  return 1

---------------------   Queue Functionality --------------------------------------------------

createTaskWithQueue :: SA.CreateTodoRequest -> F.Flow (SA.CreateTodoResponse)
createTaskWithQueue req@SA.CreateTodoRequest {task,description} = do
  now <- liftIO $ DateTime.getCurrentTimeIST
  id <- liftIO $ UUID.toText <$> UUID.nextRandom
  let status = "PENDING"
      respBody = SA.CreateTodoResponse id task description status now
  _ <- QT.createTask req
  CM.void $ liftIO $ STE.sleep 0.03
  kvInsert <- KVQ.setExKey task $ SA.CreateTodoResponse id task description status now
  dummyCall <- liftIO $ Dummy.sendAck
  forkThreadHelper numberofThreads id
  return respBody

pushThreadIntoQueue :: Int -> DT.Text -> F.Flow ()
pushThreadIntoQueue threadCount parentThreadId =  do
  TA.Env{callBackQueue} <- MTR.ask
  liftIO $ mapM_ (\x ->
    writeIntoQueue callBackQueue $ Unsafe.unsafeCoerce (threadCallerFunction ("parentThreadId : " <> parentThreadId <> " threadNumber : " <> (DT.pack $ show x)) )) [1..threadCount]


writeIntoQueue :: (TA.CallbackQueue a) -> (IO a) -> IO ()
writeIntoQueue chan flow = UChan.writeChan (fst chan) flow

