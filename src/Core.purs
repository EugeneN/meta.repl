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
appEffectsLogic uiChannel (AppState s) = runAff handleError handleResult $ do
  liftEff $ setBusy
  input <- case ds of
              Just ds' -> readSource ds'
              Nothing  -> pure Nothing
  let internal = fromMaybe Nothing $ callProcessor <$> proc <*> input
  liftEff $ setContent internal
  pure "unit wtf" -- wtf?

  where
  currentNode = findChildNodeByPath s.currentPath appDNA
  ds = getDataSource <$> currentNode
  proc = getProcessor <$> currentNode

  setBusy :: Eff _ Unit
  setBusy         = send uiChannel $ RenderState $ (AppState s{currentContent = Just "###### ![...](ajax-loader.gif) Loading from Github..."})

  setContent :: Maybe Internal -> Eff _ Unit
  setContent (Just (Md x)) = send uiChannel $ RenderState (AppState s{currentContent = Just x})
  setContent _             = send uiChannel $ RenderState (AppState s{currentContent = Nothing})
  handleError e   = setContent $ Just $ Md $ toString e
  handleResult x  = pure unit -- setContent $ Just $ Md (x <> " lo lo")

  mdImg s = "# ![" <> s <> "](" <> s <> ")"

  callProcessor :: Processor -> Input -> Maybe Internal
  callProcessor MdProcessor      (StringInput s)  = Just $ Md s
  callProcessor TextProcessor    (StringInput s)  = Just $ Md s
  callProcessor ImgListProcessor (StringInput s)  = Just $ Md $ mdImg s
  callProcessor ImgListProcessor (ArrayInput ss)  = Just $ Md $ unlines $ mdImg <$> ss
  callProcessor _ _                               = Nothing

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

appLogic :: BLActions -> AppState -> AppState
appLogic (Navigate path) (AppState s) =
  let mbNode = findChildNodeByPath path appDNA
      mbSource = getDataSource <$> mbNode
      newPath = case mbSource of
            Just (ChildSource p) -> path <> [p]
            _                    -> path
  in AppState (s { actionsCount = s.actionsCount + 1, currentPath  = newPath })

appLogic Noop            (AppState s) = AppState (s { actionsCount = s.actionsCount + 1 })

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

calcTitle :: AppState -> String
calcTitle appState =
  joinWith " <*> " [(fromMaybe "404" $ getTitle <$> getCurrentNode appState ), (getTitle appDNA)]


-- readSource (StringSource x) = x

getTitle :: Node -> String
getTitle (Node x) = x.title

getPath :: Node -> String
getPath  (Node x) = x.path

getDataSource :: Node -> DataSource String
getDataSource (Node x) = x.dataSource

getProcessor :: Node -> Processor
getProcessor (Node x) = x.processor
