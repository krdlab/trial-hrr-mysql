{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module BlogEntry where

import Prelude hiding (id)
import DataSource (defineTable)
import Database.Record.TH (derivingShow)

$(defineTable [] "test" "blog_entry" [derivingShow])
