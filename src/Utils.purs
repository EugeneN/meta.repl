module Utils where

import Control.Monad.Eff (Eff())
import Prelude
import DOM (DOM(..))
import Data.Maybe
import Types
import Data.String (joinWith)


unlines = joinWith "\n"

-- | Returns url's query parameters by name
foreign import getParameterByName :: forall e. String -> Eff e String

getParameterByName' q = do
  x <- getParameterByName q
  pure $ case x of
    "" -> Nothing
    _  -> Just x

foreign import injectBody :: forall e. String -> Eff e Unit

foreign import toString :: forall a. a -> String

foreign import setTitle :: forall eff. String -> Eff (dom :: DOM | eff) Unit

foreign import platformDetect :: forall e. Eff e String

platformDetect' :: Eff _ Platform
platformDetect' = do
  p <- platformDetect

  pure $ case p of
    "browser" -> Browser
    "nodejs"  -> Nodejs
    _         -> Unknown
