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


data UiParam = HtmlUI | ConsoleUI | TelnetUI

parseUiParam = do
  x <- getParameterByName' "ui"
  pure $ case x of
    Just "html"    -> Just HtmlUI
    Just "console" -> Just ConsoleUI
    Just "telnet"  -> Just TelnetUI
    _              -> Nothing

setupUI appSignal actionsChannel uiParam = do
  uiChannel <- case uiParam of
    HtmlUI    -> setupHtmlUi   actionsChannel
    ConsoleUI -> setupCliUi    actionsChannel
    TelnetUI  -> setupTelnetUi actionsChannel

  runSignal (appSignal ~> (appEffectsLogic uiChannel))

main = do
  platform <- platformDetect'
  log $ "platform " ++ show platform

  uiParam <- case platform of
    Browser -> do
      x <- parseUiParam
      pure $ Just $ fromMaybe ConsoleUI x

    Nodejs  -> pure $ Just TelnetUI
    _       -> pure Nothing

  actionsChannel <- channel Noop

  let actionsSignal = subscribe actionsChannel
  let appSignal = foldp appLogic initialState actionsSignal

  case uiParam of
    Just x     -> setupUI appSignal actionsChannel x
    Nothing    -> log "Error: No interface."
