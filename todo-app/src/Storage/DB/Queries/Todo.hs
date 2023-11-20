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

todoTable :: Text -> B.DatabaseEntity be DB.TodoDB (B.TableEntity Todo.TodosT)
todoTable  = DB.todo . DB.todoDB "Todos"

createTask :: SA.CreateTodoRequest -> F.Flow ()
createTask req@SA.CreateTodoRequest {task,description} = do
  todoId <- liftIO $ UUID.toText <$> UUID.nextRandom
  now <- liftIO $ DateTime.getCurrentTimeIST
  let todo :: TT.Todos = TT.Todos todoId task description "PENDING" True now now Nothing
  runQuery $ insertRow (todoTable "public") $ TT.insertExpressions [ todo ]
  return ()

fetchAllRow dbEntity = B.runSelectReturningList $ B.select $ B.all_ dbEntity

fetchAlltask :: F.Flow [TT.Todos]
fetchAlltask = runQuery $ fetchAllRow (todoTable "public")

insertRow ::
    (B.Beamable table, be ~ BP.Postgres)
  => B.DatabaseEntity be DB.TodoDB (B.TableEntity table)
  -> B.SqlInsertValues be (table (B.QExpr be s)) -> BP.Pg ()
insertRow dbEntity tableRow = B.runInsert $ B.insert dbEntity tableRow

runQuery :: BP.Pg a -> F.Flow a
runQuery query = do
  conn <- DBConf.dbGetConnection
  liftIO $ BP.runBeamPostgres conn query

-- updateTask tableName value = do

-- findTask tableName value = do

-- findAllTask tableName value= do