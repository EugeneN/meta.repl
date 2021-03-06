module Core where

import Control.Monad.Aff
import Control.Monad.Aff.Par
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Aff.Class

import Control.Alt(Alt, (<|>))
import Control.Monad.Eff.Exception(error)
import Control.Monad.Error.Class(throwError)

import qualified Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith, drop, take)
import Data.String.Regex
import Data.Traversable
import Data.Tuple

import Network.HTTP.Affjax

import Prelude

import Signal.Channel (send, Channel())

import Types
import Data
import Utils
import Processors.Blog.Main (blogProcessor)
import Processors.ImgList.Main (imgListProcessor)
import Processors.PlainText.Main (textProcessor)

import Control.Monad.Maybe.Trans
import Data.Foldable (for_)


appEffectsLogic :: Channel UIActions -> AppState -> Eff _ Unit
appEffectsLogic uiChannel apst@(AppState s) = runAppEffects do
  liftEff $ setBusy

  for_ ds \d -> do
    input <- readSource d
    for_ input \i -> do
      for_ proc \p -> do
        internal <- callProcessor p apst i
        case internal of
          Just (Cmd x) -> liftEff $ setCmd x
          _            -> liftEff $ setContent internal

  where
  getCurrentNode :: AppState -> Maybe Node
  getCurrentNode (AppState s) = s.currentNode
  runAppEffects x = runAff handleError handleResult x

  currentNode = getCurrentNode apst
  ds          = getDataSource <$> currentNode
  proc        = getProcessor <$> currentNode

  setBusy :: Eff _ Unit
  setBusy = send uiChannel $ RenderState $ (AppState s{currentContent = Just (Md "###### ![...](ajax-loader.gif) Loading...")})

  setContent :: Maybe Internal -> Eff _ Unit
  setContent x   = send uiChannel $ RenderState (AppState s{currentContent = x})

  setCmd x   = send uiChannel $ SetCmd x apst

  handleError e  = setContent $ Just $ Md $ toString e
  handleResult x = pure unit

  callProcessor :: Processor -> AppState -> Input -> Aff _ (Maybe Internal)
  callProcessor MdProcessor apst i      = textProcessor i apst
  callProcessor TextProcessor apst i    = textProcessor i apst
  callProcessor ImgListProcessor apst i = imgListProcessor i apst
  callProcessor BlogProcessor apst i    = blogProcessor i apst

  callProcessor _ _ _                   = pure Nothing

  readSource :: DataSource String -> Aff _ (Maybe Input)
  readSource (StringSource a) = pure $ Just $ StringInput a
  readSource (ArraySource as) = pure $ Just $ ArrayInput as
  readSource (GistSource gid) = do
    res <- loadGist gid
    pure $ Just $ StringInput $ parseGistResponse res.response

  readSource (GithubSource url) = do
    res <- get url
    pure $ Just $ StringInput res.response

  readSource _ = pure Nothing

  loadGist gid = get $ "https://api.github.com/gists/" <> gid

findTargetNode path parent = do
  {head: h, tail: t} <- A.uncons path
  node <- findChildNodeByPath [h] parent
  case getPathProcessor node of
    ChildPP  -> pure (Tuple node t)
    GlobalPP -> if t == [] then pure (Tuple node [])
                           else findTargetNode t node

appLogic :: BLActions -> AppState -> AppState
appLogic (Navigate path) (AppState s) =
  case findTargetNode path appDNA of
    Nothing -> AppState (s { actionsCount = s.actionsCount + 1
                           , currentPath = path
                           , menuPath = path
                           , keyboardInput = Nothing
                           , currentNode = Nothing })
    Just (Tuple targetNode pathTail) ->
      let newPath = case getDataSource targetNode of
                      ChildSource p -> path <> [p]
                      _             -> pathTail
          menuPath = case getDataSource targetNode of
                          ChildSource p -> path <> [p]
                          _             -> path
          newTargetNode = case getDataSource targetNode of
                          ChildSource p -> findChildNodeByPath [p] targetNode
                          _             -> Just targetNode
      in AppState (s { actionsCount = s.actionsCount + 1
                     , currentPath = newPath
                     , menuPath = menuPath
                     , keyboardInput = Nothing
                     , currentNode = newTargetNode })

appLogic (KeyboardInput k) (AppState s) = AppState (s { keyboardInput = Just k
                                                      , actionsCount = s.actionsCount + 1 })
appLogic Noop              (AppState s) = AppState (s { actionsCount = s.actionsCount + 1 })

getCurrentNode :: AppState -> Maybe Node
getCurrentNode appState = findChildNodeByPath (getCurrentPath appState) appDNA

getChildNodes :: Node -> Array String
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

getCurrentPath :: AppState -> Array String
getCurrentPath (AppState s) = s.currentPath

getMenuPath :: AppState -> Array String
getMenuPath (AppState s) = s.menuPath

calcTitle :: AppState -> String
calcTitle appState =
  joinWith " <*> " [(fromMaybe "Hi" $ getTitle <$> getCurrentNode appState ), (getTitle appDNA)]

calcPageUrl :: AppState -> Url
calcPageUrl (AppState s) = "#!" <> (joinWith "/" s.menuPath) -- add base url

calcPageId :: AppState -> String
calcPageId (AppState s) = "#!" <> (joinWith "/" s.menuPath)

getTitle :: Node -> String
getTitle (Node x) = x.title

getPath :: Node -> String
getPath  (Node x) = x.path

getDataSource :: Node -> DataSource String
getDataSource (Node x) = x.dataSource

getProcessor :: Node -> Processor
getProcessor (Node x) = x.processor

getPathProcessor (Node x) = x.pathProcessor
