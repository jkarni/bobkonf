module FromJSON where

import Verdict
import Verdict.JSON

type Age = Validated (Minimum 0 :&& Maximum 200) Int

data Person = Person
    { name :: String
    , age :: Age
    } deriving (Eq, Show, Read, FromJSON)
