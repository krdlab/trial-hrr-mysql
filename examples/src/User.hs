{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module User where

import Prelude hiding (id)
import DataSource (defineTable)
import Database.Record.TH (derivingShow)

$(defineTable [] "test" "user" [derivingShow])
