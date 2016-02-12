module Main where

import Verdict
import Data.Aeson
import Data.Swagger

data User = User { name :: String, age :: Age }
  deriving (Eq, Show, Read, FromJSON, ToSchema)

jsonEG :: Either String User
jsonEG = eitherDecode "{ \"name\": \"Julian K. Arni\", \"age\": -20 }"

main :: IO ()
main = print jsonEG
