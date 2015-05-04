module Main where

import Database.HDBC (runRaw, quickQuery', fromSql)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.Relational.Query
    (query, relation, aggregateRelation, wheres, (.=.), (.>=.), (!), (><), value, count, min', groupBy, desc, id', relationalQuery, Relation)

import Data.Int (Int32, Int64)
import Data.List (isInfixOf)
import Data.Time (Day, fromGregorian)

import DataSource (connect)
import User (user, User)
import qualified User as U

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
    setSqlMode conn
    printQuery users
    printResult conn users
  where
    printQuery q =
        putStrLn $ "Query : " ++ show (relationalQuery q)
    printResult c q = do
        r <- runQuery' c (relationalQuery q) ()
        putStrLn $ "Result: " ++ show r

    users :: Relation () User
    users = relation $ query user

    setSqlMode conn = do
        mode <- quickQuery' conn "SELECT @@SESSION.sql_mode" []
        newmode <- case mode of
            [[sqlval]] ->
                let val = fromSql sqlval in
                    if "IGNORE_SPACE" `isInfixOf` val
                        then return val
                        else return $ val ++ ",IGNORE_SPACE"
            _          ->
                error "failed to get 'sql_mode'"
        runRaw conn $ "SET SESSION sql_mode = '" ++ newmode ++ "'"

