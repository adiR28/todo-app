{-# LANGUAGE AllowAmbiguousTypes #-}
module Storage.DB.Queries.Todo where

import qualified Database.Beam.Postgres as BP
import qualified Storage.DB.Types.DB as DB
import qualified Database.Beam as B
import qualified Storage.DB.Types.Todo as Todo
import qualified Storage.Types.API as SA
import qualified Storage.DB.Types.Todo as TT
import Data.Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Utils.DateTime as DateTime
import qualified Storage.DB.DBConfig as DBConf
import Control.Monad.IO.Class
import qualified Flow as F
-- import qualified Database.Beam.Backend.SQL.BeamExtensions as B
import qualified OpenTelemetry.Instrumentation.Beam as OIB


todoTable :: Text -> B.DatabaseEntity be DB.TodoDB (B.TableEntity Todo.TodosT)
todoTable  = DB.todo . DB.todoDB "Todos"

createTask :: SA.CreateTodoRequest -> F.Flow ()
createTask req@SA.CreateTodoRequest {task,description} = do
  todoId <- liftIO $ UUID.toText <$> UUID.nextRandom
  now <- liftIO $ DateTime.getCurrentTimeIST
  let todo :: TT.Todos = TT.Todos todoId task description "PENDING" True now now Nothing
  runQuery $ insertRow (todoTable "public") $ TT.insertExpressions [ todo ]
  return ()

updateTask :: Text -> SA.UpdateTodoRequest -> F.Flow ()
updateTask taskId (req@SA.UpdateTodoRequest{task,description,active,status}) = do 
  _ <- runQuery $ 
          updateRow 
              (todoTable "public") 
              (\TT.Todos{..} -> 
                  mconcat
                    [
                      _task B.<-. B.val_ task ,
                      _description B.<-. B.val_ description,
                      _active B.<-.  B.val_ active,
                      _status B.<-. B.val_ status
                    ])
              (\TT.Todos{..} -> _id B.==. B.val_ taskId )      
  return ()          

fetchAlltask :: F.Flow [TT.Todos]
fetchAlltask = runQuery $ fetchAllRow (todoTable "public")

insertRow ::
    (B.Beamable table, be ~ BP.Postgres)
  => B.DatabaseEntity be DB.TodoDB (B.TableEntity table)
  -> B.SqlInsertValues be (table (B.QExpr be s)) -> BP.Pg ()
insertRow dbEntity tableRow = B.runInsert $ B.insert dbEntity tableRow

-- fetchAllRow ::
--      (B.Beamable table , be ~ BP.Postgres)
--   => B.DatabaseEntity be DB.TodoDB (B.TableEntity table)
--   -> BP.Pg [TT.Todos] -- find its Return Type

fetchAllRow dbEntity = B.runSelectReturningList $ B.select $ B.all_ dbEntity


-- update
updateRow :: 
     (B.Beamable table, be ~ BP.Postgres)
  => B.DatabaseEntity be DB.TodoDB (B.TableEntity table)
  -> (forall s. table (B.QField s) -> B.QAssignment be s)
  -- ^ A sequence of assignments to make.
  -> (forall s. table (B.QExpr be s) -> B.QExpr be s Bool)
  -> BP.Pg ()
--     forall table. (B.Beamable table, B.FromBackendRow (be ~ BP.Postgres) (table Identity)) 
--   => B.DatabaseEntity be DB.TodoDB (B.TableEntity table) 
--   -> BP.Pg [table Identity]
updateRow dbEntity val cond = B.runUpdate $ B.update dbEntity val cond 


runQuery :: BP.Pg a -> F.Flow a
runQuery query = do
  conn <- DBConf.dbGetConnection
  OIB.wrapBeamBackend conn query

-- updateTask tableName value = do

-- findTask tableName value = do

-- findAllTask tableName value= do