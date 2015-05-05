{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Comment where

import Prelude hiding (id)
import DataSource (defineTable)
import Database.Record.TH (derivingShow)

$(defineTable [] "test" "comment" [derivingShow])
