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
import qualified Utils.Transformer as TF
import qualified Data.Text as DT
import qualified Benchmark.Models as BM
import qualified Utils.Jsonifier as UJ
import qualified Data.Text.Encoding as DTE
import qualified Jsonifier as J
import qualified Control.DeepSeq as CD
import qualified Data.ByteString.Lazy as BSL
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
  -- let toJSONValtesting = Aeson.toJSON respBody
  -- let toJSONFierValtesting = UJ.toJsonifier respBody
  -- _ <- liftIO $ putStrLn $ "toJSONtesting value = " <> show toJSONValtesting
  -- _ <- liftIO $ putStrLn $ "toJSONFiertesting value = " <> show toJSONFierValtesting
  -- _ <- liftIO $ putStrLn $ "---------------------------------"
  -- let toJSONValStatus = Aeson.toJSON status
  -- let toJSONFierValStatus = UJ.toJsonifier status
  -- _ <- liftIO $ putStrLn $ "toJSONStatus value = " <> show toJSONValStatus
  -- _ <- liftIO $ putStrLn $ "toJSONFierStatus value = " <> show toJSONFierValStatus
  -- _ <- liftIO $ putStrLn $ "---------------------------------"
  -- let toJSONValCTX = Aeson.toJSON ctx
  -- let toJSONFierValCTX = UJ.toJsonifier ctx
  -- _ <- liftIO $ putStrLn $ "toJSONValCTX value = " <> show toJSONValCTX
  -- _ <- liftIO $ putStrLn $ "toJSONFierValCTX value = " <> show toJSONFierValCTX
  kvInsert <- KVQ.setExKey task $ SA.CreateTodoResponse id task description status' now (Right 1)
  dummyCall <- liftIO $ Dummy.sendAck

  return respBody

-- updateTask

-- deleteTask


-- mkDummyJsonifierExp == SA.JsonifierExp
-- mkDummyJsonifierExp = SA.JsonifierExp "aditya" True (Just "Tetsing") (Aeson.String "value") "now" ["hello"] Nothing Nothing [] (Left "abc")

sampleData :: BM.Result
sampleData = 
    BM.Result
    {
      results =  [
        BM.Story
        {
          from_user_id_str = "3646730",
          profile_image_url = "http=//a3.twimg.com/profile_images/404973767/avatar_normal.jpg",
          created_at= "Wed, 26 Jan 2011 04=35=07 +0000",
          from_user= "nicolaslara",
          id_str= "30121530767708160",
          metadata= BM.Metadata 
            {
              result_type= "recent"
            },
          to_user_id= Just 18616016,
          text= "@josej30 Python y Clojure. Obviamente son diferentes, y cada uno tiene sus ventajas y desventajas. De Haskell faltaría pattern matching",
          id= 30121530767708160,
          from_user_id= 3646730,
          -- id= "josej30",
          geo= Nothing,
          iso_language_code= "es",
          to_user_id_str= Just "18616016",
          source= "&lt;a href=&quot;http=//twitter.com/&quot; rel=&quot;nofollow&quot;&gt;Twitter for iPhone&lt;/a&gt;"
        }
      ],
      max_id = 30121530767708160,
      since_id= 0,
      refresh_url= "?since_id=30121530767708160&q=haskell",
      next_page= "?page=2&max_id=30121530767708160&rpp=100&q=haskell",
      results_per_page= 100,
      page= 1,
      completed_in= 1.195569,
      since_id_str= "0",
      max_id_str= "30121530767708160",
      query= "haskell"
    }


loadTest :: F.Flow DT.Text
loadTest = do
  let sampleData100 = replicate 100 sampleData
  -- let convert = CD.rnf $ DTE.decodeUtf8 $ J.toByteString $ UJ.toJsonifier sampleData100
  let convert = CD.rnf $ DTE.decodeUtf8 $ BSL.toStrict $ Aeson.encode sampleData100
  return "ok"

