{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Exception (bracket)
import Data.Char (toLower)
import Data.Function ((&))
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ.Interpolated

data Person = Person {name :: String, age :: Integer}

instance SQL.ToRow Person where
  toRow p = SQL.toRow (name p, age p)

table :: String
table = "people"

main :: IO ()
main = bracket (SQL.open ":memory:") SQL.close $ \conn -> do
  print [isql|CREATE TABLE !{table} (name TEXT, age INTEGER)|]
  conn & [iexecute|CREATE TABLE !{table} (name TEXT, age INTEGER)|]

  let name = "clive"
  print [isql|INSERT INTO !{table} VALUES ({name}, 40)|]
  conn & [iexecute|INSERT INTO !{table} VALUES ({name}, 40)|]

  let clara = Person {name = "clara", age = 25}
  print [isql|INSERT INTO !{table} VALUES @{clara}|]
  conn & [iexecute|INSERT INTO !{table} VALUES @{clara}|]

  ageHaskellSum <- conn & [ifold|SELECT age FROM !{table}|] 0 (\acc (SQL.Only x) -> pure (acc + x))
  print (ageHaskellSum :: Int)

  let minAge = 1 :: Int
  print [isql|SELECT AVG(age) FROM !{table} WHERE age >= {minAge}|]
  [ageAvg] <- conn & [iquery|SELECT AVG(age) FROM !{table} WHERE age >= {minAge}|]
  print (ageAvg :: SQL.Only Double)
