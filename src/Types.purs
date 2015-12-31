module Types where

import Prelude
import Signal.DOM (CoordinatePair())
import Data.Maybe (Maybe())

import Signal.Channel (Channel())
import Control.Monad.Eff (Eff())

import Text.Smolder.Markup


-- | Here is the specification for inter-component APIs
-- | aka the heart of the application :-)

data BLActions = Navigate (Array Url) | Noop
data UIActions = RenderState AppState | RenderNoop

type UIInterface blActions uiActions uiEff = Channel blActions -> uiEff (Channel uiActions)
type UnitInterface parentActions unitActions unitEff = Channel parentActions -> unitEff (Channel unitActions)

type UI eff = UIInterface BLActions UIActions eff

-- | Application core's types

data AppState = AppState {
    actionsCount :: Int
  , currentPath  :: Array Url
  , currentContent :: Maybe Internal
}

data Node = Node {
    title      :: String
  , path       :: Url
  , children   :: Array Node
  , dataSource :: DataSource String
  , processor  :: Processor
  , pathProcessor :: PathProcessor
  -- , processors :: Array Processor
}

data Processor = MdProcessor | ImgListProcessor | TextProcessor | BlogProcessor

data PathProcessor = GlobalPP | ChildPP

type Url = String

data DataSource a = StringSource a | ArraySource (Array a) | ChildSource a | GistSource a | GithubSource a

data Input = StringInput String | ArrayInput (Array String)
data Internal = Md String | HTML Markup

data Platform = Browser | Nodejs | Unknown

instance showPlatform :: Show Platform where
  show Browser = "Browser"
  show Nodejs  = "Nodejs"
  show Unknown = "Unknown"
