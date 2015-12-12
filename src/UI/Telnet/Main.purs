module UI.Telnet.Main (setupTelnetUi) where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..))

import Signal (foldp, runSignal, filter)
import Signal.Channel (channel, subscribe, send, Channel())

import Text.Markdown.SlamDown.Pretty
import Text.Markdown.SlamDown.Parser

import Data
import Types
import Core
import Utils

data UIState = UIState { text  :: String
                       , title :: String }

setupTelnetUi :: Channel Input -> Eff _ (Channel UIActions)
setupTelnetUi inputChannel = do

  renderChan <- channel RenderNoop
  -- let renderSignal = subscribe renderChan
  -- let initialUIState = UIState { text: "", title: "<$>" }
  -- let ui = foldp uiLogic initialUIState renderSignal

  log "Telnet UI here"

  -- runSignal (printPage <$> ui)
  pure renderChan
