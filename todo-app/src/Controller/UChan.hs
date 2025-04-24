module Controller.UChan where

import Prelude
import qualified Control.Concurrent.Chan.Unagi.Bounded as UChan
import qualified Control.Monad as CM
import qualified Control.Concurrent as Concurrent
-- import qualified Flow as F
import qualified Control.Exception as CE
import qualified Data.Text as DT
import qualified Storage.Types.App as TA
import qualified Data.Monoid as DM

queueSize :: Int
queueSize = 10000


-- TODO do test for multiple worker threads (2,4)
initiateChan :: IO (TA.CallbackQueue DM.Any,[Concurrent.ThreadId])
initiateChan = do
    chan@(_ , outChan) <- UChan.newChan queueSize
    threadId <- Concurrent.forkIO $ CM.forever $ chanWorker =<< UChan.readChan outChan
    return (chan,[threadId])

-- since callbacks are fire and forgot type, so use of MVar to hold state is redundant
chanWorker :: (IO DM.Any) -> IO (Either DT.Text DM.Any )
chanWorker flow = do
    (eitherResp :: Either CE.SomeException a) <- CE.try flow
    case eitherResp of
        Left err -> return $ Left $ DT.pack $ show err
        Right resp -> return $ Right $ resp