module Storage.DB.Types.Todo where

import Data.Aeson
import Data.Time
import qualified Database.Beam as BM
import qualified Database.Beam.Schema.Tables as B
import qualified Storage.DB.Utils as DU
import Data.Text
import Utils.Jsonifier
import qualified Utils.Jsonifier as UJ
import qualified Utils.UtilsTH as UTH

data TodosT f =
  Todos 
    {
      _id :: B.C f Text,
      _task :: B.C f Text,
      _description :: B.C f (Maybe Text),
      _status :: B.C f Text, -- TODO add enum
      _active :: B.C f Bool,
      _createdAt :: B.C f LocalTime,
      _updatedAt :: B.C f LocalTime,
      _udf :: B.C f (Maybe Value)
    } deriving (BM.Generic,B.Beamable)

type Todos = TodosT BM.Identity

type TodosPrimary = B.PrimaryKey TodosT BM.Identity

instance B.Table TodosT where
  data PrimaryKey TodosT f = TodosPrimary (B.C f Text)
                             deriving (BM.Generic, B.Beamable)
  primaryKey = TodosPrimary . _id

deriving instance Show Todos

deriving instance Eq Todos

instance FromJSON Todos where
  parseJSON = genericParseJSON DU.stripAllLensPrefixOptions

instance ToJSON Todos where
  toJSON =
    genericToJSON (DU.stripAllLensPrefixOptions {omitNothingFields = True})

todoEMod :: 
    Text 
 -> Text 
 -> B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity TodosT)
todoEMod tableName schema = 
 B.setEntitySchema (Just schema) <>
 B.setEntityName tableName <>
 B.modifyTableFields
   B.tableModification
     { _id = "id"
     , _task = "task"
     , _description = "description"
     , _status = "status"
     , _active = "active"
     , _createdAt = "createdAt"
     , _updatedAt = "updatedAt"
     , _udf = "udf"
     }

insertExpression c = insertExpressions [c]

insertExpressions cs = BM.insertExpressions (toRowExpression <$> cs)
 where
   toRowExpression Todos {..} =
     Todos
       (BM.val_ _id)
       (BM.val_ _task)
       (BM.val_ _description)
       (BM.val_ _status)
       (BM.val_ _active)
       (BM.val_ _createdAt)
       (BM.val_ _updatedAt)
       (BM.val_ _udf)


-- $(UTH.mkJsonifierInstanceTH ''TodosT)