module DataSource (connect, defineTable) where

import Database.HDBC.MySQL (Connection, connectMySQL, MySQLConnectInfo(..), defaultMySQLConnectInfo)
import Database.HDBC.Query.TH (defineTableFromDB')
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.MySQL (driverMySQL)
import Database.Relational.Query (Config (..), defaultConfig)
import Language.Haskell.TH (Q, Dec, TypeQ)
import Language.Haskell.TH.Name.CamelCase (ConName)

connect :: IO Connection
connect = connectMySQL info
  where
    info = defaultMySQLConnectInfo {
          mysqlUser     = "hrr-tester"
        , mysqlPassword = ""
        , mysqlDatabase = "test"
        , mysqlHost     = "hrr-trial-mysql-server"
        }

config :: Config
config =  defaultConfig { normalizedTableName = False }

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap = defineTableFromDB' connect config (driverMySQL { typeMap = tmap })
