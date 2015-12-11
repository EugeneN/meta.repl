module Main where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())

import Signal (foldp, runSignal)
import Signal.Channel (channel, subscribe, send)

import Types
import Data
import Core
import Utils
import UI.Console.Main (setupCliUi)
import UI.HTML.Main    (setupHtmlUi)


main = do
  uiParam <- getParameterByName "ui" -- from env, args, not exactly document.location

  actionsChannel <- channel Noop

  let actionsSignal = subscribe actionsChannel
  let app = foldp appLogic initialState actionsSignal

  uiChannel <- case uiParam of
    "html"    -> setupHtmlUi actionsChannel
    "console" -> setupCliUi  actionsChannel
    _         -> setupCliUi  actionsChannel

  runSignal ((\s -> send uiChannel $ RenderState s) <$> app)
