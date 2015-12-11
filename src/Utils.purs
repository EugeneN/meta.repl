module Utils where

import Control.Monad.Eff (Eff())
import Prelude
import DOM (DOM(..))
import Types

-- | Returns url's query parameters by name
foreign import getParameterByName :: forall e. String -> Eff e String

foreign import injectBody :: forall e. String -> Eff e Unit

foreign import toString :: forall a. a -> String

foreign import setTitle :: forall eff. String -> Eff (dom :: DOM | eff) Unit

readSource (MemorySource x) = x
getTitle (Node x) = x.title
getPath  (Node x) = x.path
