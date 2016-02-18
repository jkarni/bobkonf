{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Main where

import           Control.Monad            (replicateM)
import           Data.Aeson               (FromJSON, ToJSON, eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BSL
import           Data.Proxy               (Proxy (..))
import           Data.Swagger             (Schema, ToSchema, toSchema)
import           GHC.Generics             (Generic)
import           Test.QuickCheck          (arbitrary, generate)

import           Verdict
import           Verdict.DB
import           Verdict.JSON             ()
import           Verdict.QuickCheck       ()

-- Datatype definitions {{{---------------------------------------------------
------------------------------------------------------------------------------

type Age = Validated (Minimum 0 :&& Maximum 200) Integer

data Person = Person
    { name :: String
    , age  :: Age
    } deriving (Eq, Show, Read, Generic, ToSchema, ToJSON, FromJSON)

examplePerson :: Person
examplePerson = Person "Julian" (unsafeValidated 15)

--}}}-------------------------------------------------------------------------

-- Do it once {{{-------------------------------------------------------------
------------------------------------------------------------------------------
-- Read {{{-------------------------------------------------------------------

readExample :: Person
readExample = read "Person {name = \"Julian K. Arni\", age = 300}"

--}}}
-- FromJSON {{{---------------------------------------------------------------

fromJSONExample :: Either String Person
fromJSONExample = eitherDecode "{ \"name\": \"Julian K. Arni\", \"age\": -20 }"

--}}}
-- JSON Schema {{{------------------------------------------------------------

jsonSchemaExample :: Schema
jsonSchemaExample = toSchema (Proxy :: Proxy Person)

jsonSchemaExample' :: IO ()
jsonSchemaExample'
  = BSL.writeFile "jsonSchemaExample.json" $ encodePretty jsonSchemaExample
--}}}
-- QuickCheck {{{-------------------------------------------------------------

arbitraryExample :: IO [Validated (Maximum 100) Integer]
arbitraryExample = generate $ replicateM 20 arbitrary

--}}}
-- DB {{{---------------------------------------------------------------------

db :: DB '[Length 5, Length 10] [Int]
db = insert [1..10] $ insert [2..6] $ insert [1..5] empty

query1 :: [Validated (Length 5) [Int]]
query1 = query db

query2 :: [Validated (Length 10) [Int]]
query2 = query db

{-
- Won't typecheck:

query3 :: [Validated (Length 7) [Int]]
query3 = query db

-}

db2 :: DB '[Length 5, Length 3] [Int]
db2 = insert [1..5] $ insert [1..10] $ insert [1..3] empty


query4 :: [Joined [Int] [Int]]
query4 = crossJoin db db2
-- }}}
--}}}-------------------------------------------------------------------------

-- As a type {{{--------------------------------------------------------------
------------------------------------------------------------------------------
-- Implication {{{------------------------------------------------------------

safeDiv :: (c `Implies` (Not (Equals 0))) =>
  Integer -> Validated c Integer -> Integer
safeDiv n d = n `div` getVal d
-- }}}
-- Inference {{{--------------------------------------------------------------

-- From Oleg Kiselyov
class Sum2 a b c | a b -> c, a c -> b
instance Sum2 Z a a
instance Sum2 a b c => Sum2 (S a) b (S c)
class Sum a b c | a b -> c, a c -> b, b c -> a
instance (Sum2 a b c, Sum2 b a c) => Sum a b c

type VInt c = Validated (Equals c) Int
type Three = S (S (S Z))
type Five = S (S Three)
add :: Sum c1 c2 c3 => VInt c1 -> VInt c2 -> VInt c3
add x y = unsafeValidated $ getVal x + getVal y

addExample :: IO ()
addExample = do
  x <- readLn
  let v = add x (read "3" :: VInt Three) :: VInt Five
  print v
-- }}}
-- }}}------------------------------------------------------------------------

-- Main {{{-------------------------------------------------------------------
------------------------------------------------------------------------------

main :: IO ()
main = return ()

-- }}}
