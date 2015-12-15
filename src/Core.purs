module Core where

import Prelude
import qualified Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)

import Types
import Data
import Utils

appLogic :: BLActions -> AppState -> AppState
appLogic (Navigate path) (AppState s) = AppState (s { actionsCount = s.actionsCount + 1
                                                    , currentPath  = path })
appLogic Noop            (AppState s) = AppState (s { actionsCount = s.actionsCount + 1 })

getCurrentNode appState =
  findChildNodeByPath (getCurrentPath appState) appDNA

getChildNodes (Node x) = x.children <#> \(Node y) -> y.path

findChildNodeByPath :: Array Url -> Node -> Maybe Node
findChildNodeByPath pathElements (Node node) = case A.uncons pathElements of
  Nothing -> Nothing
  Just {head: path0, tail: []}               -> find path0 node.children
  Just {head: path0, tail: pathElementsTail} -> case find path0 node.children of
    Nothing          -> Nothing
    Just currentNode -> findChildNodeByPath pathElementsTail currentNode

  where
  find path_ nodes = A.head $ A.filter (match path_) nodes
  match p (Node y) = y.path == p

getCurrentPath (AppState s) = s.currentPath

calcTitle appState =
  joinWith " <*> " [(fromMaybe "404" $ getTitle <$> getCurrentNode appState ), (getTitle appDNA)]

readSource (MemorySource x) = x
getTitle (Node x) = x.title
getPath  (Node x) = x.path
