module Types where

import Prelude
import Signal.DOM (CoordinatePair())
import Data.Maybe (Maybe())

import Signal.Channel (Channel())
import Control.Monad.Eff (Eff())
import Control.Monad.Aff
import Control.Monad.Aff.AVar (AVAR())
import Network.HTTP.Affjax (AJAX())

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
    actionsCount   :: Int
  , currentPath    :: Array Url
  , menuPath       :: Array Url
  , currentNode    :: Maybe Node
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

instance showNode :: Show Node where
  show (Node a) = "Node { title: " <> a.title
                    <> ", path: " <> a.path
                    <> ", dataSource: " <> show a.dataSource
                    <> ", processor: " <> show a.processor
                    <> ", pathProcessor: " <> show a.pathProcessor
                    <> ", children: " <> show a.children
                    <> " }"

data Processor = MdProcessor | ImgListProcessor | TextProcessor | BlogProcessor

type ProcessorAPI = forall e. Input -> AppState -> Aff ( ajax :: AJAX, avar :: AVAR | e) (Maybe Internal)

instance showProcessor :: Show Processor where
  show MdProcessor = "MdProcessor"
  show ImgListProcessor = "ImgListProcessor"
  show TextProcessor = "TextProcessor"
  show BlogProcessor = "BlogProcessor"

data PathProcessor = GlobalPP | ChildPP

instance showPathProcessor :: Show PathProcessor where
  show GlobalPP = "GlobalPP"
  show ChildPP = "ChildPP"

type Url = String

data DataSource a = StringSource a | ArraySource (Array a) | ChildSource a | GistSource a | GithubSource a

instance showDataSource :: Show (DataSource String) where
  show (StringSource a) = "<StringSource " <> show a <> ">"
  show (ChildSource a) = "<ChildSource " <> show a <> ">"
  show (GistSource a) = "<GistSource " <> show a <> ">"
  show (GithubSource a) = "<GithubSource " <> show a <> ">"
  show (ArraySource a) = "<ArraySource " <> show a <> ">"

data Input = StringInput String | ArrayInput (Array String)
data Internal = Md String | HTML Markup

data Platform = Browser | Nodejs | Unknown

instance showPlatform :: Show Platform where
  show Browser = "Browser"
  show Nodejs  = "Nodejs"
  show Unknown = "Unknown"
