module UI.HTML.Main (setupHtmlUi) where

import Prelude hiding (div, map, sub)

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())

import DOM (DOM())
import Signal.Channel (Chan())

import Data.Foldable (for_)
import Data.Array (length, (!!), uncons)
import Data.List hiding (head, span, length, (!!), uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.String (split, joinWith)

import qualified DOM.Node.Types as DT

import Routing (hashChanged)

import Signal (foldp, runSignal)
import Signal.Channel (channel, subscribe, send, Channel())

import Text.Markdown.SlamDown.Pretty
import Text.Markdown.SlamDown.Parser
import Text.Markdown.SlamDown

import Text.Smolder.HTML hiding (title)
import Text.Smolder.HTML.Attributes (href, className, src, lang, charset, name, content, rel, title)
import Text.Smolder.Markup
import Text.Smolder.Renderer.String (render)

import VirtualDOM
import VirtualDOM.VTree

import Core
import Data
import Types
import Utils
import Internal
import UI.HTML.Utils
import qualified UI.HTML.VDom as VDom


page404 = toHtml <<< parseMd $ "> ## 404 Not found"
initialVDom = vNode2vTree $ VDom.render $ div $ text "One moment please"
defaultTitle = "Eugene Naumenko" -- read from appDNA

data UIState = UIState { rootNode    :: DT.Node
                       , title       :: String
                       , oldVDom     :: VTree
                       , newVDom     :: VTree }

-- uiLogic :: UIActions AppState -> UIState -> UIState
uiLogic (RenderState appState) (UIState u) =
  UIState (u { oldVDom = u.newVDom
             , title   = calcTitle appState
             , newVDom = newVDom })
  where
  newMarkup = renderHTML appState
  newVDom = vNode2vTree $ VDom.render newMarkup

uiLogic RenderNoop uiState = uiState

type HtmlUiEff a = forall e. Eff (console :: CONSOLE, dom :: DOM, chan :: Chan | e) a

setupHtmlUi :: UI HtmlUiEff
setupHtmlUi inputChannel = do
    let rootNode = createElement initialVDom
    appendToBody rootNode

    renderChan <- channel RenderNoop
    let renderSignal = subscribe renderChan
    let initialUIState = UIState { rootNode: rootNode
                                 , oldVDom: initialVDom
                                 , title: defaultTitle
                                 , newVDom: initialVDom }
    let ui = foldp uiLogic initialUIState renderSignal

    runSignal (patchVDom <$> ui)

    hashChanged $ \old new -> do
      log $ "hash changed: " ++ old ++ " -> " ++ new
      send inputChannel $ Navigate $ split "/" new
      pure unit

    pure renderChan

patchVDom :: UIState -> Eff _ Unit
patchVDom (UIState s) = do
  let patches = diff s.oldVDom s.newVDom
  newRootNode <- patch s.rootNode patches

  setTitle s.title

  pure unit

renderMenu :: Array Url -> Maybe Node -> Array String -> Int -> Markup
renderMenu fullPath Nothing baseUrl level = mempty
renderMenu fullPath (Just node) baseUrl level = case uncons fullPath of
    Nothing                  -> drawNodeMenu (Just node) baseUrl Nothing level

    Just {head: h, tail: []} -> do
      drawNodeMenu (Just node) baseUrl (Just h) level
      drawNodeMenu (findChildNodeByPath [h] node) (baseUrl <> [h]) (Just h) (level + 1)

    Just {head: h, tail: t}  -> do
      drawNodeMenu (Just node) baseUrl (Just h) level
      renderMenu t (findChildNodeByPath [h] node) (baseUrl <> [h]) (level + 1)

    where
    drawNodeMenu Nothing baseUrl' selected level = mempty
    drawNodeMenu (Just node) baseUrl' selected level = do
      div ! className ("menu-level-" <> show level <> " " <> if level > 0 then "sub-menu" else "") $ do
        for_ (getMenuItems node) $ \(MenuItem slug title) ->
          case selected of
            Nothing -> a ! href (makeUrl baseUrl' slug) $ text title
            Just selected' ->
              if slug == selected'
                then a ! className "current-menu-item" ! href (makeUrl baseUrl' slug) $ text title
                else a ! href (makeUrl baseUrl' slug) $ text title

    makeUrl base_url slug = "#" <> (joinWith "/" (base_url <> [slug]))

renderHTML :: AppState -> Markup
renderHTML appState@(AppState s) =
  div ! className "content" $ do
    div ! className "section" $ do
      div ! className "mode-menu-toolbar" $ do
        a ! className "text mode-menu" ! href "?ui=console" $ text "REPL mode"
        a ! className "text mode-menu" ! href "app.js" ! title "To use CLI/telnet mode, please run `app.js` with Node.js and then connect to it with telnet or netcat" $ text "CLI/telnet mode"
      -- h1 ! className "name" $ text (getTitle appDNA)

      div ! className "nav" $ do
        renderMenu menuPath (Just appDNA) [] 0

      div ! className "section page"  $ do
        --h2 $ text $ fromMaybe "-no title-" $ getTitle <$> getCurrentNode appState
        div ! className "text" $ do
          payloadHtml

        -- div ! className "section footer" $ do
          -- span $ text "© 2015"

  where
  menuPath = getMenuPath appState
  currentNode = getCurrentNode appState
  internalAST = fromMaybe page404 $ parseContent <$> s.currentContent
  payloadHtml = internalAST
