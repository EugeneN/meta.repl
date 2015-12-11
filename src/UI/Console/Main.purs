module UI.Console.Main (setupCliUi) where

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

foreign import exportGlobal :: forall e. String -> (String -> Eff e Unit) -> Eff e Unit


data UIState = UIState { text :: String }

setupCliUi :: Channel Input -> Eff _ (Channel UIActions)
setupCliUi inputChannel = do
  renderChan <- channel RenderNoop
  let renderSignal = subscribe renderChan
  let initialUIState = UIState { text: "" }
  let ui = foldp uiLogic initialUIState renderSignal

  exportGlobal "go" $ \x -> send inputChannel $ Navigate [x]
  runSignal (printPage <$> ui)
  pure renderChan

printPage :: UIState -> Eff _ Unit
printPage (UIState s) = log s.text

uiLogic (RenderState appState) (UIState u) = UIState (u { text = renderPage appState })
uiLogic RenderNoop             uiState     = uiState

renderPage appState = header ++ showPage (getCurrentNode appState) ++ footer appState

showPage :: Maybe Node -> String
showPage Nothing = "404 No such page"
showPage (Just (Node x)) = formatPage x.title x.dataSource

formatPage title (MemorySource body) = prettyPrintMd <<< parseMd $ "#" ++ title ++ "\n\n" ++ body
formatPage title _                   = prettyPrintMd <<< parseMd $ "#" ++ title ++ "\n\n-no data-"

header              = "\n\n"
footer (AppState s) =  "\n\n(c) 2015"
                    ++ "\n\n-------------------------------------------------"
                    ++ "\nActions count: " ++ show s.actionsCount
                    ++ "\nEnter `go(<page>)` to navigate to the respective page"
                    ++ "\nAvailable pages: " ++ show (getChildNodes theSite)
