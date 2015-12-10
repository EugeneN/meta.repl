module UI.Console.Main (renderCLI) where

import Prelude
import Signal.Channel (send, Channel())
import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..))

import Text.Markdown.SlamDown.Pretty
import Text.Markdown.SlamDown.Parser

import Data
import Types
import Core

foreign import exportGlobal :: forall e. String -> (String -> Eff e Unit) -> Eff e Unit

showNode :: Maybe Node -> String
showNode Nothing = "404 No such page"
showNode (Just (Node x)) = formatText x.title x.dataSource

formatText title (MemorySource body) =
  "\n\n" ++ (prettyPrintMd <<< parseMd) (title ++ "\n========\n\n" ++ body) ++ "\n(c) 2015\n\n"
formatText title _ =
  "\n\n" ++ (prettyPrintMd <<< parseMd) (title ++ "\n========\n\n-no data-") ++ "\n(c) 2015\n\n"

-- | Will be called on every render
-- | TODO: Implement setupUI and UI state
renderCLI :: Channel Input -> AppState -> Eff _ Unit
renderCLI inputChannel appState@(AppState s) = do
  log $ showNode (getCurrentNode appState)

  log   "-------------------------------------------------"
  log $ "Actions count: " ++ show s.actionsCount
  log   "Enter `go(<node>)()` to go to the respective node"
  log $ "Available nodes: " ++ show (getChildNodes theSite)

  exportGlobal "go" $ \x -> send inputChannel $ Navigate [x]
