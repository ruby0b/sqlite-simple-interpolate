{-# LANGUAGE TemplateHaskell #-}

-- | Interpolated SQL queries
module Database.SQLite.Simple.QQ.Interpolated
  ( isql
  , quoteInterpolatedSql
  , iquery
  , iexecute
  , ifold
  ) where

import Language.Haskell.TH (Exp, Q, appE, listE, sigE, tupE, varE, litE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lit(..), lookupValueName)
import Database.SQLite.Simple.ToField (toField)
import Text.Parsec (ParseError)
import Database.SQLite.Simple
import Data.Foldable (foldrM)
import Data.String (fromString)

import Database.SQLite.Simple.QQ.Interpolated.Parser (StringPart (..), parseInterpolated)

-- | Quote a SQL statement with embedded antiquoted expressions.
--
-- The result of the quasiquoter is a tuple, containing the statement string and a list
-- of parameters. For example:
--
-- @[isql|SELECT field FROM table WHERE name = ${map toLower "ELLIOT"} LIMIT ${10}|]@
--
-- produces
--
-- @("SELECT field FROM table WHERE name = ? LIMIT ?", [toField ((map toLower) "ELLIOT"), toField 10])@
--
-- How the parser works:
--
-- Any expression occurring between @${@ and @}@ will be replaced with a @?@
-- and passed as a query parameter.
--
-- Characters preceded by a backslash are treated literally. This enables the
-- inclusion of the literal substring @${@ within your quoted text by writing
-- it as @\\${@. The literal sequence @\\${@ may be written as @\\\\${@.
--
-- Note: This quasiquoter is a wrapper around 'Database.SQLite.Simple.QQ.sql'.
--
-- This quasiquoter only works in expression contexts and will throw an error
-- at compile time if used in any other context.
isql :: QuasiQuoter
isql = QuasiQuoter
  { quoteExp = quoteInterpolatedSql
  , quotePat = error "isql quasiquoter does not support usage in patterns"
  , quoteType = error "isql quasiquoter does not support usage in types"
  , quoteDec = error "isql quasiquoter does not support usage in declarations"
  }

combineParts :: [StringPart] -> Q ([Q Exp], [Q Exp])
combineParts = foldrM step ([], [])
  where
    step subExpr (s, exprs) = case subExpr of
      AntiInject e -> injectExpr e (s, exprs)
      Lit str -> pure (litE (StringL str) : s, exprs)
      Esc c -> pure (litE (StringL [c]) : s, exprs)
      AntiParam e -> pure (litE (StringL "?") : s, e : exprs)

injectExpr :: String -> ([Q Exp], [Q Exp]) -> Q ([Q Exp], [Q Exp])
injectExpr name (s, exprs) = do
  valueName <- lookupValueName name
  case valueName of
    Nothing ->
      fail $ "Value `" ++ name ++ "` is not in scope"
    Just found -> do
      pure (varE found : s, exprs)

applySql :: [StringPart] -> Q Exp
applySql parts = do
  (queryParts, exps) <- combineParts parts
  let queryStr = appE [| fromString :: String -> Query |] $ appE [| concat |] $ listE queryParts
  tupE [queryStr, sigE (listE $ map (appE (varE 'toField)) exps) [t| [SQLData] |]]

-- | The internal parser used by 'isql'.
quoteInterpolatedSql :: String -> Q Exp
quoteInterpolatedSql s = either (handleError s) applySql (parseInterpolated s)

handleError :: String -> ParseError -> Q Exp
handleError expStr parseError = error $ mconcat
  [ "Failed to parse interpolated expression in string: "
  , expStr
  , "\n"
  , show parseError
  ]

-- | Invokes 'query' with arguments provided by 'isql'.
-- The result is of type '(Connection -> IO [r])'.
iquery :: QuasiQuoter
iquery = isql { quoteExp = appE [| \(q, qs) c -> query c q qs |] . quoteInterpolatedSql }

-- | Invokes 'execute' with arguments provided by 'isql'
-- The result is of type '(Connection -> IO ())'.
iexecute :: QuasiQuoter
iexecute = isql { quoteExp = appE [| \(q, qs) c -> execute c q qs |] . quoteInterpolatedSql }

-- | Invokes 'fold' with arguments provided by 'isql'.
-- The result is of type 'a -> (a -> row -> IO a) -> Connection -> IO a'.
ifold :: QuasiQuoter
ifold = isql { quoteExp = appE [| \(q, qs) acc f c -> fold c q qs acc f |] . quoteInterpolatedSql }
