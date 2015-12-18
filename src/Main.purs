module Main where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())

import Data.Maybe

import Signal (foldp, runSignal, (<~))
import Signal.Channel (channel, subscribe, send, Channel())

import Types
import Data
import Core
import Utils
import UI.Console.Main (setupCliUi)
import UI.HTML.Main    (setupHtmlUi)
import UI.Telnet.Main    (setupTelnetUi)

import Control.Monad.Aff
import Control.Monad.Eff.Class
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Response
import Network.HTTP.Affjax.Request

setupUI app actionsChannel ui = do
  uiChannel <- case ui of
    "html"    -> setupHtmlUi   actionsChannel
    "console" -> setupCliUi    actionsChannel
    "telnet"  -> setupTelnetUi actionsChannel

  -- runSignal ((\s -> send uiChannel $ RenderState s) <~ app)
  runSignal ((appEffectsLogic uiChannel) <~ app)


appEffectsLogic :: Channel UIActions -> AppState -> Eff _ Unit
appEffectsLogic uiChannel (AppState s) = case aff of
  Nothing -> send uiChannel $ RenderState (AppState s{currentContent = Nothing})
  Just aff' -> runAff (\_ -> send uiChannel $ RenderState (AppState s{currentContent = Nothing}))
                      (\x -> send uiChannel $ RenderState (AppState s{currentContent = Just x}))
                      aff'
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

  applyProcessor GistProcessor (StringSource s) = do
    res <- loadGist s
    pure res.response

  applyProcessor GistProcessor (ArraySource s) = pure "ArraySource not supported for gist"

  mdImg s = "![" <> s <> "](" <> s <> ")"

  loadGist gid = get $ "https://api.github.com/gists/" <> gid



main = do
  platform <- platformDetect'
  log $ "platform " ++ show platform

  uiParam <- case platform of
    Browser -> do
      x <- getParameterByName' "ui"
      pure $ Just $ fromMaybe "console" x

    Nodejs  -> pure $ Just "telnet"
    _       -> pure Nothing

  actionsChannel <- channel Noop

  let actionsSignal = subscribe actionsChannel
  let app = foldp appLogic initialState actionsSignal

  case uiParam of
    Just ui    -> setupUI app actionsChannel ui
    Nothing    -> log "Error: No interface."
