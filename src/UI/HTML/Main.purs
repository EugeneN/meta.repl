module UI.HTML.Main (renderHTML) where

import Prelude
import Signal.Channel (send, Channel())
import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..))

import Text.Markdown.SlamDown.Pretty
import Text.Markdown.SlamDown.Parser

import Text.Smolder.HTML (html, head, meta, link, title, body, h1, p, pre, a)
import Text.Smolder.HTML.Attributes (lang, charset, httpEquiv, content, name, rel, href)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.Renderer.String (render)

import Data
import Types
import Core

foreign import setInnerHTML :: forall e. String -> Eff e Unit

showNode :: Maybe Node -> String
showNode Nothing = "404 No such page"
showNode (Just (Node x)) = formatText x.title x.dataSource

formatText title (MemorySource body) =
  "\n\n" ++ (prettyPrintMd <<< parseMd) (title ++ "\n========\n\n" ++ body) ++ "\n(c) 2015\n\n"
formatText title _ =
  "\n\n" ++ (prettyPrintMd <<< parseMd) (title ++ "\n========\n\n-no data-") ++ "\n(c) 2015\n\n"

getTitle (Node x) = x.title

-- | Will be called on every render
-- | TODO: Implement setupUI and UI state
renderHTML :: Channel Input -> AppState -> Eff _ Unit
renderHTML inputChannel appState@(AppState s) = do
  let slam = showNode (getCurrentNode appState)
  let doc = html ! lang "en" $ do
              head $ do
                meta ! charset "utf-8"
                title $ text (getTitle theSite)
                meta ! name "description" ! content "YES OMG HAI LOL"
                meta ! name "viewport" ! content "width=device-width"
                link ! rel "stylesheet" ! href "css/screen.css"
              body $ do
                a ! href "?ui=console" $ text "Console UI"
                h1 $ text (getTitle theSite)
                pre $ text $ slam
                            ++ "-------------------------------------------------"
                            ++ "Actions count: " ++ show s.actionsCount
                            ++ "Available nodes: " ++ show (getChildNodes theSite)

  log slam
  setInnerHTML $ render doc
