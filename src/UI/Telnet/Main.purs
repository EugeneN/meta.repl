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

import Node.Net.Socket
import Control.Apply
import Node.ReadLine

host = "localhost" :: String
port = 8888 :: Int

data UIState = UIState { text  :: String
                       , title :: String }

foreign import data Process :: !
foreign import exit :: forall eff. Int -> Eff (process :: Process | eff) Unit

telnet :: forall eff. String -> Int -> Eff ( console :: CONSOLE, socketio :: SocketIO | eff) Unit
telnet host port = do
  sock <- createServer defaultServerOptions
  -- setNoDelay true sock
  onError (\e -> log (show e) *> exit 1) sock
  onEnd (log "connection closed" *> exit 0) sock
  onData log sock

  listenTCP host port sock
  log $ "Server started on " ++ host ++ ":" ++ show port

  -- interface <- createInterface noCompletion
  -- setPrompt ">" 0 interface
  -- setLineHandler interface (\l -> write (l++"\n") (return unit) sock)
  return unit

setupTelnetUi :: Channel Input -> Eff _ (Channel UIActions)
setupTelnetUi inputChannel = do

  renderChan <- channel RenderNoop
  -- let renderSignal = subscribe renderChan
  -- let initialUIState = UIState { text: "", title: "<$>" }
  -- let ui = foldp uiLogic initialUIState renderSignal

  log "Telnet UI here"

  telnet host port



  -- start socket connection on some port, listen to commands and print pages

  -- runSignal (printPage <$> ui)
  pure renderChan
