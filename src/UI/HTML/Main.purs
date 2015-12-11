module UI.HTML.Main (setupHtmlUi, UIActions(..)) where

import Prelude hiding (div, map, sub)
import Signal.Channel (send, Channel())
import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.List hiding (head, span)
import Data.Foldable (for_)

import Text.Markdown.SlamDown.Pretty
import Text.Markdown.SlamDown.Parser
import Text.Markdown.SlamDown

import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes (href, className, src, lang, charset, name, content, rel)
import Text.Smolder.Markup
import Text.Smolder.Renderer.String (render)

import VirtualDOM
import VirtualDOM.VTree

import Routing (hashChanged)

import Signal (foldp, runSignal)
import Signal.Channel (channel, subscribe, send)

import qualified DOM.Node.Types as DT

import Data
import Types
import Core

import qualified UI.HTML.VDom as VDom

foreign import injectBody :: forall e. String -> Eff e Unit
foreign import toString :: forall a. a -> String

readSource (MemorySource x) = x

page404 = parseMd "## 404 Not found"

parseBody (Node x) = parseMd <<< readSource $ x.dataSource

class ToHtml a where
  toHtml :: a -> Markup

instance toHtmlSlamDown :: ToHtml SlamDown where
  toHtml (SlamDown bs) = toHtml bs

instance toHtmlListBlock :: ToHtml (List Block) where
  toHtml as = p  $ do for_ as toHtml

instance toHtmlBlock :: ToHtml Block where
  toHtml (Paragraph is)              = p  $ for_ is toHtml
  toHtml (Header n is) | n == 1      = h1 $ for_ is toHtml
                            | n == 2 = h2 $ for_ is toHtml
                            | n == 3 = h3 $ for_ is toHtml
                            | n == 4 = h4 $ for_ is toHtml
                            | n == 5 = h5 $ for_ is toHtml
                            | n == 6 = h6 $ for_ is toHtml
  toHtml (Blockquote bs)             = blockquote $ for_ bs toHtml
  toHtml (Lst (Bullet _) bss)        = ul $ for_ bss toHtml
  toHtml (Lst (Ordered _) bss)       = ol $ for_ bss toHtml
  toHtml (CodeBlock _ ss)            = div ! className "code" $ text $ show ss
  toHtml (LinkReference l uri)       = a ! href uri $ text $ l
  toHtml Rule                        = hr

instance toHtmlInline :: ToHtml Inline where
  toHtml (Str s)                     = text $ s
  toHtml (Entity s)                  = text $ s
  toHtml Space                       = text $ " "
  toHtml SoftBreak                   = text $ "&shy;"
  toHtml LineBreak                   = br
  toHtml (Emph is)                   = em     $ for_ is toHtml
  toHtml (Strong is)                 = strong $ for_ is toHtml
  toHtml (Code e s)                  = div ! className "inline-code" $ text $ s
  toHtml (Link is (InlineLink uri))           = a ! href uri $ for_ is toHtml
  toHtml (Link is (ReferenceLink Nothing))    = a ! href "#" $ for_ is toHtml
  toHtml (Link is (ReferenceLink (Just uri))) = a ! href uri $ for_ is toHtml
  toHtml (Image is uri)              = img ! src uri
  toHtml (FormField s b ff)          = text $ "[form-field " ++ show s ++ " " ++ show b ++ " " ++ show ff ++ "]"

foreign import vNode2vTree :: VDom.VNode -> VTree

data MenuItem = MenuItem Url String

getTitle (Node x) = x.title
getPath  (Node x) = x.path

getMenuItems (Node x) = x.children <#> \(Node y) -> MenuItem y.path y.title

initialVDom = vNode2vTree $ VDom.render $ div $ text "initial vdom"

foreign import appendToBody :: forall e. DT.Node -> Eff e Unit

data UIState = UIState { rootNode    :: DT.Node
                       , oldVDom     :: VTree
                       , newVDom     :: VTree }

-- move to ui api
data UIActions = RenderState AppState | RenderNoop


-- uiLogic :: UIActions AppState -> UIState -> UIState
uiLogic (RenderState appState) (UIState u) =
  UIState (u { oldVDom = u.newVDom
             , newVDom = newVDom })
  where
  newMarkup = renderHTML appState
  newVDom = vNode2vTree $ VDom.render newMarkup

uiLogic RenderNoop uiState = uiState

setupHtmlUi :: Channel Input -> Eff _ (Channel UIActions)
setupHtmlUi inputChannel = do
    let rootNode = createElement initialVDom
    appendToBody rootNode

    renderChan <- channel RenderNoop
    let renderSignal = subscribe renderChan
    let initialUIState = UIState { rootNode: rootNode
                                 , oldVDom: initialVDom
                                 , newVDom: initialVDom }
    let ui = foldp uiLogic initialUIState renderSignal

    runSignal (patchVDom <$> ui)

    hashChanged $ \old new -> do
      log $ "hash changed: " ++ old ++ " -> " ++ new
      send inputChannel $ Navigate [new]
      pure unit

    pure renderChan

patchVDom (UIState s) = do
  let patches = diff s.oldVDom s.newVDom
  newRootNode <- patch s.rootNode patches

  pure unit

renderHTML :: AppState -> Markup
renderHTML appState@(AppState s) =
  div ! className "content" $ do
    div ! className "section" $ do
      a ! className "text mode-menu" ! href "?ui=console" $ text "CLI mode"
      h1 ! className "name" $ text (getTitle theSite)

      div ! className "nav" $ do
        for_ (getMenuItems theSite) $ \(MenuItem slug title) -> do
          if slug == currentPath
            then a ! className "current-menu-item" ! href ("#" ++ slug) $ text title
            else a ! href ("#" ++ slug) $ text title

      div ! className "section page"  $ do
        --h2 $ text $ fromMaybe "-no title-" $ getTitle <$> getCurrentNode appState
        div ! className "text" $ do
          payloadHtml

        div ! className "section footer" $ do
          span $ text "&copy; 2015"

  where
  markdownAST = fromMaybe page404 $ parseBody <$> (getCurrentNode appState)
  payloadHtml = toHtml markdownAST
  currentPath = fromMaybe "404" $ getPath <$> getCurrentNode appState
