module Utils.Transformer where

import qualified Storage.DB.Types.Todo as TT
import qualified Storage.Types.API as SA


mkFetchAllTaskResponse :: [TT.Todos] -> SA.FetchAllTaskResponse
mkFetchAllTaskResponse req = do
  let fetchResponsePayload = map (\x -> SA.FetchResponsePayload (TT._id x) (TT._task x) (TT._description x) (TT._status x) ) req
  SA.FetchAllTaskResponse fetchResponsePayload

-- mkUpdateTaskResponse :: TT.Todos -> SA.CreateTodoResponse
-- mkUpdateTaskResponse req = SA.CreateTodoResponse (TT._id req) (TT._task req) (TT._description req) (TT._status req) (TT._active req) (TT._updatedAt req)