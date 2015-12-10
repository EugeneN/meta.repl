module Utils where

import Control.Monad.Eff (Eff())

-- | Returns url's query parameters by name
foreign import getParameterByName :: forall e. String -> Eff e String
