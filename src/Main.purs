module Main where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())

import Data.Maybe

import Signal (foldp, runSignal)
import Signal.Channel (channel, subscribe, send)

import Types
import Data
import Core
import Utils
import UI.Console.Main (setupCliUi)
import UI.HTML.Main    (setupHtmlUi)
import UI.Telnet.Main    (setupTelnetUi)

setupUI app actionsChannel ui = do
  uiChannel <- case ui of
    "html"    -> setupHtmlUi   actionsChannel
    "console" -> setupCliUi    actionsChannel
    "telnet"  -> setupTelnetUi actionsChannel

  runSignal ((\s -> send uiChannel $ RenderState s) <$> app)

main = do
  platform <- platformDetect'
  log $ "platform " ++ show platform

  uiParam <- case platform of
    Browser -> getParameterByName' "ui" -- from env, args, not exactly document.location
    Nodejs  -> pure $ Just "telnet" -- get from argv
    _       -> pure Nothing

  actionsChannel <- channel Noop

  let actionsSignal = subscribe actionsChannel
  let app = foldp appLogic initialState actionsSignal

  case uiParam of
    Just ui    -> setupUI app actionsChannel ui
    Nothing    -> log "Error: No interface."
