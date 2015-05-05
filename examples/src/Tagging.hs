{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Tagging where

import Prelude hiding (id)
import DataSource (defineTable)
import Database.Record.TH (derivingShow)

$(defineTable [] "test" "tagging" [derivingShow])
