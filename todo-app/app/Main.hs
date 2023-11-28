module Main (main) where

import App.App (runServer)
import Prelude
import qualified Benchmark.Main as Bench

main :: IO ()
main = Bench.main