module UI.Console.Main (setupCliUi) where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..))
import DOM (DOM())

import Signal (foldp, runSignal, filter)
import Signal.Channel (channel, subscribe, send, Channel(), Chan())

import Text.Markdown.SlamDown.Pretty
import Text.Markdown.SlamDown.Parser

import Data
import Types
import Core
import Utils

foreign import exportGlobal :: forall e. String -> (Array String -> Eff e Unit) -> Eff e Unit


data UIState = UIState { text  :: String
                       , title :: String }


type ConsoleUiEff a = forall e. Eff (console :: CONSOLE, dom :: DOM, chan :: Chan | e) a

setupCliUi :: UI ConsoleUiEff
setupCliUi inputChannel = do
  injectBody (unlines [
      "<div class='repl-message'>"
    , "<p>Application is in REPL interface mode.</p>"

    , "<p>To use the aplication please open browser console and follow prompts. "
    , "Alternatively switch to <a href='?ui=html#about'>HTML</a> or "
    , "<a href='app.js'>CLI/telnet</a> mode.</p>"

    , "<p>For CLI/telnet mode please run `app.js` with Node.js and "
    , "connect to it with telnet or netcat.</p>"
    , "</div>"
    ])

  renderChan <- channel RenderNoop
  let renderSignal = subscribe renderChan
  let initialUIState = UIState { text: "", title: "<$>" }
  let ui = foldp uiLogic initialUIState renderSignal

  exportGlobal "go" $ \x -> send inputChannel $ Navigate x
  runSignal (printPage <$> ui)
  pure renderChan

printPage :: UIState -> Eff _ Unit
printPage (UIState s) = do
  setTitle s.title
  log s.text

uiLogic (RenderState appState) (UIState u) = UIState (u { text = renderPage appState
                                                        , title = calcTitle appState })
uiLogic RenderNoop             uiState     = uiState

renderPage appState = header ++ showPage (getCurrentNode appState) ++ footer appState

showPage :: Maybe Node -> String
showPage Nothing = "404 No such page"
showPage (Just (Node x)) = formatPage x.title x.dataSource

formatPage title (StringSource body) = prettyPrintMd <<< parseMd $ "#" ++ title ++ "\n\n" ++ body
formatPage title _                   = prettyPrintMd <<< parseMd $ "#" ++ title ++ "\n\n-no data-"

header              = "\n\n"
footer (AppState s) =  "\n\n(c) 2015"
                    ++ "\n\n-------------------------------------------------"
                    ++ "\nActions count: " ++ show s.actionsCount
                    ++ "\nEnter `go([<page>])` to navigate to the respective page"
                    ++ "\nAvailable pages: " ++ show (getChildNodes appDNA)
