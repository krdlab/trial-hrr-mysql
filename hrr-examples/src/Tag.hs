{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Tag where

import Prelude hiding (id)
import DataSource (defineTable)
import Database.Record.TH (derivingShow)

$(defineTable [] "test" "tag" [derivingShow])
