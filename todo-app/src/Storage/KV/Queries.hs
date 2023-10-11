module Storage.KV.Queries where

import qualified Database.Redis as R
import qualified Storage.KV.KVConf as KVConf
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString.Lazy as BSL
import Data.Text
import qualified Flow as F
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Exception as Exp
import qualified Control.Exception as CE

setExKey :: forall a.
  A.ToJSON a 
  => Text 
  -> a
  -> F.Flow ()
setExKey key value = do
  conn <- KVConf.kvGetConnection
  resp' <- liftIO $ R.runRedis conn $ R.set (DTE.encodeUtf8 key) (BSL.toStrict $ A.encode value)
  resp <- liftIO $ R.runRedis conn $ R.expire (DTE.encodeUtf8 key) 86400
  return ()

fetchKey :: forall a.
  (A.ToJSON a,
  A.FromJSON a)
  => Text
  -> F.Flow (Maybe a)
fetchKey key = do
  conn <- KVConf.kvGetConnection
  resp <- liftIO $ R.runRedis conn $ R.get (DTE.encodeUtf8 key)
  case resp of
    Left err -> CE.throw $ Exp.Exception $ "Unable to fetch from redis :" <> show err
    Right res -> 
        case res of
          Nothing -> return Nothing
          Just resp -> 
            case A.decodeStrict resp of
              Nothing -> CE.throw $ Exp.Exception $ "Unable to decode Aeson :" 
              Just r -> return $ Just r

  
deleteKey :: Text -> F.Flow (Bool)
deleteKey key =  do
  conn <- KVConf.kvGetConnection
  resp <- liftIO $ R.runRedis conn $ R.del [(DTE.encodeUtf8 key)]
  case resp of
    Left err -> CE.throw $ Exp.Exception $ "Unable to delete key :"  <> show key <> " " <> show err
    Right val -> if val == 1 then return True else return False

-- iskeyExists key = do
--   conn <- KVConf.kvGetConnection
--   R.exists (DTE.encodeUtf8 key)