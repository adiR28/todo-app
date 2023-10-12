module Utils.Utils where

-- import Data.Text

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (a:_) = Just a