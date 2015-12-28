module Processors.PlainText.Main (textProcessor) where


import Control.Monad.Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class

import Data.Maybe (Maybe(..))

import Prelude



import Types
import Utils


textProcessor :: Input -> Aff _ (Maybe Internal)
textProcessor (StringInput s)   = pure $ Just $ Md s
