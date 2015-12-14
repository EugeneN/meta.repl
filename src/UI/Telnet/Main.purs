module UI.Telnet.Main (setupTelnetUi) where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..))
import Data.String (trim)

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

onError'      = flip onError
onData'       = flip onData
onClose'      = flip $ onEvent "close"
onEnd'        = flip $ onEvent "end"
onListening'  = flip $ onEvent "listening"
onConnection' = flip onConnection

data UIState = UIState { text  :: String
                       , title :: String }

foreign import data Process :: !
foreign import exit :: forall eff. Int -> Eff (process :: Process | eff) Unit

clientHandler :: Signal UIState -> Channel Input -> Socket -> Eff _ Unit
clientHandler ui inputChannel clientSocket = do
  log "Got a client"
  runSignal ((printPage clientSocket) <$> ui) -- FIXME TODO runSignal once per ui, read value for each client separately

  onError' clientSocket $ \e ->
    log ("Error: " ++ show e)

  onData' clientSocket $ \x -> do
    let cmd = trim $ toString x
    log $ "Page request: >" ++ cmd ++ "<"
    if cmd == "bye" then end clientSocket
                    else send inputChannel $ Navigate [cmd]
    pure unit

  onClose' clientSocket $ \_ -> log "Connection interrupted"
  onEnd'   clientSocket $ \_ -> log "Client disconnected"

  pure unit

  where
  printPage :: Socket -> UIState -> Eff _ Unit
  printPage clientSocket (UIState s) = do
    write s.text (pure unit) clientSocket
    pure unit

startServer :: forall eff. String -> Int -> Signal UIState -> Channel Input -> Eff ( console :: CONSOLE, socketio :: SocketIO | eff) Unit
startServer host port ui inputChannel = do
  sock <- createServer defaultServerOptions

  onError'      sock $ \e -> log ("General error: " ++ (show e)) *> exit 1
  onConnection' sock clientHandler'
  onListening'  sock $ \_ -> log "Listening..."

  listenTCP host port sock
  log $ "Server started on " ++ host ++ ":" ++ show port

  pure unit

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
                    ++ "\nEnter page name to navigate to the respective page,"
                    ++ "\nor `bye` to disconnect."
                    ++ "\nAvailable pages: " ++ show (getChildNodes appDNA)
                    ++ "\n\nEnter your choice: "
