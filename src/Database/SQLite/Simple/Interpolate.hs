-- | Interpolated SQLite queries
module Database.SQLite.Simple.Interpolate (
  isql,
  iquery,
  iexecute,
  ifold,
  quoteInterpolatedSql,
) where

import CustomInterpolation
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (toField)
import GHC.OldList (intercalate)
import Language.Haskell.TH (Exp, Q, appE, listE, sigE, stringE, tupE, varE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

{- | Quote an SQL statement with embedded antiquoted expressions.

The result of the quasiquoter is a tuple, containing the statement string and a list
of parameters. For example:

>>> import Data.Char (toLower)
>>> [isql|SELECT field FROM !{map toLower "PEOPLE"} WHERE name = {map toLower "ELLIOT"} AND access IN @{["admin", "employee"]} LIMIT {10 :: Int}|]
("SELECT field FROM people WHERE name = ? AND access IN (?,?) LIMIT ?",[SQLText "elliot",SQLText "admin",SQLText "employee",SQLInteger 10])

The generated code is:

@("SELECT field FROM people WHERE name = ? AND access IN (?,?) LIMIT ?", ['toField' (map toLower "ELLIOT")] ++ 'toRow' ["admin", "employee"] ++ ['toField' (10 :: Int)])@

How the parser works:

* Any expression occurring between @{@ and @}@ will be replaced with a @?@
and passed as a query parameter using 'toField'.
* Any expression occuring between @\@{@ and @}@ will be replaced with the right amount of @?@, separated by commas and surrounded by parentheses (e.g. @(?,?,?)@ for a `ToRow` instance with 3 fields).
The expression gets converted to query parameters using 'toRow'.
* Any expression occurring between @!{@ and @}@ will be replaced with its value, bypassing the anti-injection mechanisms. /Never use this one for user input!/

Characters preceded by a backslash are treated literally. This enables the
inclusion of the literal character @{@ within your quoted text by writing
it as @\\{@. The literal sequence @\\{@ may be written as @\\\\{@.
-}
isql :: QuasiQuoter
isql =
  QuasiQuoter
    { quoteExp = quoteInterpolatedSql,
      quotePat = error "This quasiquoter does not support usage in patterns",
      quoteType = error "This quasiquoter does not support usage in types",
      quoteDec = error "This quasiquoter does not support usage in declarations"
    }

-- | The Template Haskell function used by 'isql'.
quoteInterpolatedSql :: String -> Q Exp
quoteInterpolatedSql =
  interpolate
    defaultConfig
      { finalize = consumeInterpolated,
        handlers =
          [ simpleInterpolator
              { prefix = "",
                handler = \sqlFieldExpr -> (Just $ listE [appE (varE 'toField) sqlFieldExpr], [|"?"|])
              },
            simpleInterpolator
              { prefix = "!",
                handler = \stringExpr -> (Nothing, stringExpr)
              },
            simpleInterpolator
              { prefix = "@",
                handler = \sqlRowExpr ->
                  let sqlRow = appE (varE 'toRow) sqlRowExpr
                      rowLength = appE (varE 'length) sqlRow
                      intercalateCommas = appE $ appE (varE 'intercalate) $ stringE ","
                      questionMarks = intercalateCommas $ appE (appE (varE 'replicate) rowLength) [|"?"|]
                      questionMarksWithParens = appE (varE 'concat) $ listE [stringE "(", questionMarks, stringE ")"]
                   in (Just sqlRow, questionMarksWithParens)
              }
          ]
      }

consumeInterpolated :: ([Maybe (Q Exp)], Q Exp) -> Q Exp
consumeInterpolated (sqlDataExprs, queryStrExpr) = tupE [queryStr, sqlData]
  where
    queryStr = appE [|fromString :: String -> Query|] queryStrExpr
    sqlData = sigE (appE (varE 'concat) (listE sqlDataExprs')) [t|[SQLData]|]
    sqlDataExprs' = catMaybes sqlDataExprs

{- | Invokes 'query' with arguments provided by 'isql'.
The result is of type @('Connection' -> 'IO' [r])@.
-}
iquery :: QuasiQuoter
iquery = isql {quoteExp = appE [|\(q, qs) c -> query c q qs|] . quoteInterpolatedSql}

{- | Invokes 'execute' with arguments provided by 'isql'.
The result is of type @('Connection' -> 'IO' ())@.
-}
iexecute :: QuasiQuoter
iexecute = isql {quoteExp = appE [|\(q, qs) c -> execute c q qs|] . quoteInterpolatedSql}

{- | Invokes 'fold' with arguments provided by 'isql'.
The result is of type @(a -> (a -> row -> 'IO' a) -> 'Connection' -> 'IO' a)@.
-}
ifold :: QuasiQuoter
ifold = isql {quoteExp = appE [|\(q, qs) acc f c -> fold c q qs acc f|] . quoteInterpolatedSql}
