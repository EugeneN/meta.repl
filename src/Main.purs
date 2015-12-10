module Main where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())


import Signal (foldp, runSignal, filter)
import Signal.Channel (channel, subscribe)

import Types
import Data
import Core
import Utils
import UI.Console.Main (renderCLI)
import UI.HTML.Main (renderHTML)


main = do
  uiParam <- getParameterByName "ui"

  actionsChan <- channel Noop

  let actionsSig = subscribe actionsChan
  let app = foldp appLogic initialState actionsSig

  let renderer = case uiParam of
                   "html"    -> renderHTML actionsChan
                   "console" -> renderCLI actionsChan
                   _         -> renderCLI actionsChan

  runSignal (renderer <$> app)
