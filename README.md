<h1 align="center">sqlite-simple-interpolate</h1>

<p align="center">
  <a href="https://hackage.haskell.org/package/sqlite-simple-interpolate"><img src="https://img.shields.io/hackage/v/sqlite-simple-interpolate" alt="Hackage"></a>
  <a href="https://github.com/ruby0b/sqlite-simple-interpolate/actions/workflows/haskell-ci.yml"><img src="https://github.com/ruby0b/sqlite-simple-interpolate/actions/workflows/haskell-ci.yml/badge.svg" alt="Build Status"></a>
  <a href="https://github.com/simmsb/calamity/blob/master/LICENSE"><img src="https://img.shields.io/github/license/ruby0b/sqlite-simple-interpolate" alt="License"></a>
  <a href="https://hackage.haskell.org/package/sqlite-simple-interpolate"><img src="https://img.shields.io/hackage-deps/v/sqlite-simple-interpolate" alt="Hackage-Deps"></a>
</p>

Write natural SQL statements in Haskell using QuasiQuoters!

The QuasiQuoters support 3 methods of interpolation that can be mixed freely:

- `{}`: injection-safe field interpolation, e.g. `{myName}` gets replaced with `?` which gets substituted with `toField myName`.
- `@{}`: injection-safe row interpolation, e.g. `@{myPerson}` gets replaced with `(?,?)` (assuming `Person` has two fields) which gets substituted with `toRow myPerson`.
- `!{}`: injection-_vulnerable_ raw string interpolation. **Never use this for user input!** Intended for use cases that the anti-injection mechanisms won't allow, e.g. table names: `!{myTableName}` gets replaced with the value of `myTableName :: String`.

```haskell
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Exception (bracket)
import Data.Char (toLower)
import Data.Function ((&))
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.Interpolate

data Person = Person {name :: String, age :: Integer}

instance SQL.ToRow Person where
  toRow p = SQL.toRow (name p, age p)

table :: String
table = "people"

main :: IO ()
main = bracket (SQL.open ":memory:") SQL.close $ \conn -> do
  -- Create a table, interpolating safe string constants like table names with !{}
  conn & [iexecute|CREATE TABLE !{table} (name TEXT, age INTEGER)|]

  -- Insert a person, safely interpolating a field using {}
  let name = "clive"
  conn & [iexecute|INSERT INTO !{table} VALUES ({name}, 40)|]

  -- Insert a person, safely interpolating an entire row type using @{} (gets replaced with "(?,?)")
  let clara = Person {name = "clara", age = 25}
  conn & [iexecute|INSERT INTO !{table} VALUES @{clara}|]

  -- Use ifold to fold some rows into their sum in haskell
  ageHaskellSum <- conn & [ifold|SELECT age FROM !{table}|] 0 (\acc (SQL.Only x) -> pure (acc + x))
  print (ageHaskellSum :: Int)

  -- Let's calculate the average age of people that are at least 20 years old
  let minAge = 20 :: Int
  [ageAvg] <- conn & [iquery|SELECT AVG(age) FROM !{table} WHERE age >= {minAge}|]
  print (ageAvg :: SQL.Only Double)

  -- You can always use 'isql' directly but you'll have to use uncurry:
  (uncurry $ SQL.execute conn) [isql|INSERT OR REPLACE INTO !{table} VALUES ({name}, 41)|]
```
