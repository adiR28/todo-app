module Benchmark.Main where

import qualified Benchmark.Models as Model
import Data.Aeson as Aeson
import qualified Criterion.Main as C
import qualified Utils.Jsonifier as UJ
import qualified Jsonifier as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T

main = do
  twitter1Data <- load "/Users/aditya.ranjan/todo-app/todo-app/src/Benchmark/Sample/twittter1.json"
  let twitter10Data = mapResultsOfResult (concat . replicate 10) twitter1Data
  let twitter100Data = mapResultsOfResult (concat . replicate 100) twitter1Data
  C.defaultMain
    [
      C.bench "aeson1" $ C.nf T.decodeUtf8 $ BSL.toStrict $ Aeson.encode twitter1Data,
      C.bench "aeson10" $ C.nf T.decodeUtf8 $ BSL.toStrict $ Aeson.encode twitter10Data,
      C.bench "aeson100" $ C.nf T.decodeUtf8 $ BSL.toStrict $ Aeson.encode twitter100Data,
      C.bench "json1" $ C.nf T.decodeUtf8 $ J.toByteString  $ UJ.toJsonifier twitter1Data,
      C.bench "json10" $ C.nf T.decodeUtf8 $ J.toByteString  $ UJ.toJsonifier twitter10Data,
      C.bench "json100" $ C.nf T.decodeUtf8 $ J.toByteString  $ UJ.toJsonifier twitter100Data
    ]


mapResultsOfResult :: ([Model.Story] -> [Model.Story]) -> Model.Result -> Model.Result
mapResultsOfResult f a =
    a {Model.results = f (Model.results a)}

load :: FilePath -> IO Model.Result
load fileName =
  Aeson.eitherDecodeFileStrict' fileName
    >>= either fail return
