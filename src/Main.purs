module Main where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())

import Data.Maybe

import Signal (foldp, runSignal, (~>))
import Signal.Channel (channel, subscribe)

import Types
import Data
import Core
import Utils
import UI.Console.Main (setupCliUi)
import UI.HTML.Main    (setupHtmlUi)
import UI.Telnet.Main  (setupTelnetUi)


data UiParam = HTML | Console | Telnet

parseUiParam = do
  x <- getParameterByName' "ui"
  pure $ case x of
    Just "html"    -> Just HTML
    Just "console" -> Just Console
    Just "telnet"  -> Just Telnet
    _              -> Nothing

setupUI appSignal actionsChannel uiParam = do
  uiChannel <- case uiParam of
    HTML    -> setupHtmlUi   actionsChannel
    Console -> setupCliUi    actionsChannel
    Telnet  -> setupTelnetUi actionsChannel

  runSignal (appSignal ~> (appEffectsLogic uiChannel))

main = do
  platform <- platformDetect'
  log $ "platform " ++ show platform

  uiParam <- case platform of
    Browser -> do
      x <- parseUiParam
      pure $ Just $ fromMaybe Console x

    Nodejs  -> pure $ Just Telnet
    _       -> pure Nothing

  actionsChannel <- channel Noop

  let actionsSignal = subscribe actionsChannel
  let appSignal = foldp appLogic initialState actionsSignal

  case uiParam of
    Just x     -> setupUI appSignal actionsChannel x
    Nothing    -> log "Error: No interface."
