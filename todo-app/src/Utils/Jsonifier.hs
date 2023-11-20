module Utils.Jsonifier where

import qualified Jsonifier as J
import Data.Aeson
import Data.Scientific (Scientific)
import qualified Data.Bifunctor as Bi
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Time.Zones.Internal as DTZI
import Data.Fixed ( Pico )
import qualified Data.Text as DT --hiding(map)
import Data.ByteString.UTF8 as DSU
import Data.Functor.Identity
import Data.Either

class Jsonifier a where
  toJsonifier :: a -> J.Json

instance Show J.Json where
  show a = DSU.toString $ J.toByteString a

instance Jsonifier Value where 
  toJsonifier val =
    case val of
      Null -> J.null
      (Array arr) -> J.array $ toJsonifier <$> arr
      (String txt) -> J.textString txt
      (Number num) -> J.scientificNumber num
      (Bool bool) -> J.bool bool
      (Object obj) -> J.object $ Bi.second toJsonifier <$> HashMap.toList obj

instance Jsonifier Bool where
  toJsonifier = J.bool

instance Jsonifier Int where
    toJsonifier = J.intNumber

instance Jsonifier Scientific where
  toJsonifier = J.scientificNumber

instance Jsonifier Integer where
  toJsonifier n = J.scientificNumber $ fromInteger n

instance Jsonifier Pico where
  toJsonifier p = toJsonifier $ DTZI.picoToInteger p

instance Jsonifier DT.Text where
    toJsonifier = J.textString
 
instance (Jsonifier a) => Jsonifier (Identity a) where
  toJsonifier = toJsonifier . runIdentity 

instance (Jsonifier a) => Jsonifier ([a]) where
  toJsonifier arr = J.array $ fmap toJsonifier arr 

instance (Jsonifier a) => Jsonifier (Maybe a) where
    toJsonifier maybeVal =
      case maybeVal of 
        Just x -> toJsonifier x
        Nothing -> J.null

instance (Jsonifier lft ,Jsonifier rgt ) => Jsonifier (Either lft rgt) where
  toJsonifier eth = 
    case eth of
      Left lft -> J.object [("Left", toJsonifier lft)]
      Right rgt -> J.object [("Right", toJsonifier rgt)]