module Core where

import Control.Monad.Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class

import qualified Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)

import Network.HTTP.Affjax

import Prelude

import Signal.Channel (send, Channel())

import Types
import Data
import Utils


appEffectsLogic :: Channel UIActions -> AppState -> Eff _ Unit
appEffectsLogic uiChannel (AppState s) = case aff of
  Nothing -> send uiChannel $ RenderState (AppState s{currentContent = Nothing})
  Just aff' -> runAff handleError handleResult aff'
  where
  currentNode = findChildNodeByPath s.currentPath appDNA
  ds = getDataSource <$> currentNode
  proc = getProcessor <$> currentNode
  aff = applyProcessor <$> proc <*> ds

  applyProcessor :: Processor -> (DataSource String) -> Aff _ String
  applyProcessor TextProcessor (StringSource s) = pure s
  applyProcessor TextProcessor (ArraySource s) = pure $ unlines s

  applyProcessor ImgListProcessor (StringSource s) = pure $ mdImg s
  applyProcessor ImgListProcessor (ArraySource s) = pure $ unlines $ mdImg <$> s

  applyProcessor GistProcessor (StringSource ss) = do
    liftEff $ send uiChannel $ RenderState
      (AppState s{currentContent = Just "###### ![...](ajax-loader.gif) Loading from Github..."})
    res <- loadGist ss
    pure $ parseGistResponse res.response

  applyProcessor GithubProcessor (StringSource url) = do
    liftEff $ send uiChannel $ RenderState
      (AppState s{currentContent = Just "###### ![...](ajax-loader.gif) Loading from Github..."})
    res <- get url
    pure $ res.response

  applyProcessor _ _ = pure "Unsupported source and processor combination"

  mdImg s = "# ![" <> s <> "](" <> s <> ")"

  loadGist gid = get $ "https://api.github.com/gists/" <> gid

  handleError e = send uiChannel $ RenderState (AppState s{currentContent = Just (toString e)})
  handleResult x = send uiChannel $ RenderState (AppState s{currentContent = Just x})



appLogic :: BLActions -> AppState -> AppState
appLogic (Navigate path) (AppState s) =
  let mbNode = findChildNodeByPath path appDNA
      mbSource = getDataSource <$> mbNode
      newPath = case mbSource of
            Just (ChildSource p) -> path <> [p]
            _                    -> path
  in AppState (s { actionsCount = s.actionsCount + 1, currentPath  = newPath })

appLogic Noop            (AppState s) = AppState (s { actionsCount = s.actionsCount + 1 })

getCurrentNode appState = findChildNodeByPath (getCurrentPath appState) appDNA
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

readSource (StringSource x) = x
getTitle (Node x) = x.title
getPath  (Node x) = x.path
getDataSource (Node x) = x.dataSource
getProcessor (Node x) = x.processor
