{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Exception (bracket)
import Data.Char (toLower)
import Data.Function ((&))
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ.Interpolated

table :: String
table = "people"

main :: IO ()
main = bracket (SQL.open ":memory:") SQL.close $ \conn -> do
  print [isql|CREATE TABLE !{table} (name TEXT, age INTEGER)|]
  conn & [iexecute|CREATE TABLE !{table} (name TEXT, age INTEGER)|]

  print [isql|INSERT INTO !{table} VALUES ("clive", 40)|]
  conn & [iexecute|INSERT INTO !{table} VALUES ("clive", 40)|]

  ageSum <- conn & [ifold|SELECT age FROM !{table}|] 0 (\acc (SQL.Only x) -> pure (acc + x))
  print (ageSum :: Int)

  let limit = 1 :: Int
  print [isql|SELECT age FROM !{table} WHERE name = ${map toLower "CLIVE"} LIMIT ${limit}|]
  ages <- conn & [iquery|SELECT age FROM !{table} WHERE name = ${map toLower "CLIVE"} LIMIT ${limit}|]
  print (ages :: [SQL.Only Int])
