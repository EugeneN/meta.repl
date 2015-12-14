module Types where

import Prelude
import Signal.DOM (CoordinatePair())
import Data.Maybe (Maybe())

import Signal.Channel (Channel())
import Control.Monad.Eff (Eff())


-- | Here is the specification for inter-component APIs
-- | aka the heart of the application :-)

data BLActions = Navigate (Array Url) | Noop
data UIActions = RenderState AppState | RenderNoop

type UIInterface blActions uiActions uiEff = Channel blActions -> uiEff (Channel uiActions)
type UnitInterface parentActions unitActions unitEff = Channel parentActions -> unitEff (Channel unitActions)

-- | Application core's types

data AppState = AppState {
    actionsCount :: Int
  , currentPath  :: Array Url
}

data Node = Node {
    title      :: String
  , path       :: Url
  , children   :: Array Node
  , dataSource :: DataSource String
  -- , processors :: Array Processor
}

type Url = String

data DataSource a = MemorySource a | LocalSource a | RemoteSource a

-- data Processor a = forall a. a -> a

data Platform = Browser | Nodejs | Unknown

instance showPlatform :: Show Platform where
  show Browser = "Browser"
  show Nodejs  = "Nodejs"
  show Unknown = "Unknown"
