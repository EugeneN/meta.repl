module Utils where

import Control.Monad.Eff (Eff())
import Prelude

-- | Returns url's query parameters by name
foreign import getParameterByName :: forall e. String -> Eff e String

foreign import injectBody :: forall e. String -> Eff e Unit
foreign import toString :: forall a. a -> String
