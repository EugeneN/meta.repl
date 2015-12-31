module Processors.PlainText.Main (textProcessor) where


import Control.Monad.Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class

import Data.Maybe (Maybe(..))

import Prelude



import Types
import Utils


textProcessor :: ProcessorAPI
textProcessor (StringInput s) _   = pure $ Just $ Md s
