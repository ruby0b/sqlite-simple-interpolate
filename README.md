# `sqlite-simple-interpolate`

Write natural SQL statements in Haskell using a QuasiQuotes!

```haskell
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Char (toLower)
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ.Interpolated
import Control.Exception (bracket)

(&) = flip ($)
infixl 1 &

main :: IO ()
main = bracket (SQL.open ":memory:") SQL.close $ \conn -> do
  conn & [iexecute|CREATE TABLE people (name TEXT, age INTEGER)|]
  conn & [iexecute|INSERT INTO people VALUES ("clive", 40)|]
  -- you can always use 'isql' directly but you'll have to use uncurry:
  (uncurry $ SQL.execute conn) [isql|INSERT INTO people VALUES ("clive", 32)|]

  ageSum <- conn & [ifold|SELECT age FROM people|] 0 (\acc (SQL.Only x) -> pure (acc + x))
  print ageSum

  let limit = 1
  ages <- conn & [iquery|SELECT age FROM people WHERE name = ${map toLower "CLIVE"} LIMIT ${limit}|]
  print (ages :: [SQL.Only Int])
```

## Acknowledgements
This library is a fork of [`postgresql-simple-interpolate`](https://github.com/3noch/postgresql-simple-interpolate), adapted for use with `sqlite-simple`.

The original itself is basically just a copy of the [`here` package](https://github.com/tmhedberg/here) by Taylor M. Hedberg with slight modifications!
