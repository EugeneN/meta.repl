module Core where

import Prelude
import qualified Data.Array as A
import Data.Maybe (Maybe(..))

import Types
import Data

appLogic :: Input -> AppState -> AppState
appLogic (Navigate path) (AppState s) = AppState (s { actionsCount = s.actionsCount + 1
                                                    , currentPath  = path })
appLogic Noop            (AppState s) = AppState (s { actionsCount = s.actionsCount + 1 })

getCurrentNode appState =
  findChildNodeByPath theSite (getCurrentPath appState)

getChildNodes (Node x) = x.children <#> \(Node y) -> y.path

findChildNodeByPath (Node x) path = case path of
  [p] -> A.head $ A.filter (pred p) x.children
  _   -> Nothing
  where
  pred p (Node y) = y.path == p

getCurrentPath (AppState s) = s.currentPath
