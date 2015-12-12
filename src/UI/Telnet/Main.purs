module UI.Telnet.Main (setupTelnetUi) where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..))

import Signal (foldp, runSignal, filter, Signal())
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

clientHandler :: Signal UIState -> Channel Input -> Socket -> Eff _ Unit
clientHandler ui inputChannel clientSocket = do
  log "Got a client"
  runSignal ((printPage clientSocket) <$> ui)

  onError (\e -> log ("Error: " ++ show e)) clientSocket
  (flip onData) clientSocket $ \x -> do
    log $ "< " ++ toString x
    send inputChannel $ Navigate [x]
    pure unit

  onEvent "close" (\_ -> log "Client disconnected") clientSocket

  pure unit

  where
  printPage :: Socket -> UIState -> Eff _ Unit
  printPage clientSocket (UIState s) = do
    write s.text (pure unit) clientSocket
    pure unit

startServer :: forall eff. String -> Int -> Signal UIState -> Channel Input -> Eff ( console :: CONSOLE, socketio :: SocketIO | eff) Unit
startServer host port ui inputChannel = do
  sock <- createServer defaultServerOptions

  onError (\e -> log ("Error: " ++ (show e)) *> exit 1) sock
  onConnection clientHandler' sock
  onEvent "listening"  (\_ -> log "Listening...") sock

  listenTCP host port sock
  log $ "Server started on " ++ host ++ ":" ++ show port

  return unit

  where
  clientHandler' :: Socket -> Eff _ Unit
  clientHandler' = clientHandler ui inputChannel

setupTelnetUi :: Channel Input -> Eff _ (Channel UIActions)
setupTelnetUi inputChannel = do

  renderChan <- channel RenderNoop
  let renderSignal = subscribe renderChan
  let initialUIState = UIState { text: "", title: "<$>" }
  let ui = foldp uiLogic initialUIState renderSignal

  log "Telnet UI here"

  startServer host port ui inputChannel

  pure renderChan



uiLogic (RenderState appState) (UIState u) = UIState (u { text = renderPage appState
                                                        , title = calcTitle appState })
uiLogic RenderNoop             uiState     = uiState

renderPage appState = header ++ showPage (getCurrentNode appState) ++ footer appState

showPage :: Maybe Node -> String
showPage Nothing = "404 No such page"
showPage (Just (Node x)) = formatPage x.title x.dataSource

formatPage title (MemorySource body) = prettyPrintMd <<< parseMd $ "#" ++ title ++ "\n\n" ++ body
formatPage title _                   = prettyPrintMd <<< parseMd $ "#" ++ title ++ "\n\n-no data-"

header              =  "\n\n"
footer (AppState s) =  "\n\n(c) 2015"
                    ++ "\n\n-------------------------------------------------"
                    ++ "\nActions count: " ++ show s.actionsCount
                    ++ "\nEnter page name to navigate to the respective page"
                    ++ "\nAvailable pages: " ++ show (getChildNodes theSite)
                    ++ "\n\nEnter your choice: "
