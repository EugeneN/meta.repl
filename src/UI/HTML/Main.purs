module UI.HTML.Main (setupHtmlUi) where

import Prelude hiding (div, map, sub, id)

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())

import DOM (DOM())
import Signal.Channel (Chan())

import Data.Foldable (for_)
import Data.Array (length, (!!), uncons)
import Data.List hiding (head, span, length, (!!), uncons, filter, drop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.String (split, joinWith, drop)

import qualified DOM.Node.Types as DT

import Routing (hashChanged)

import Signal (foldp, runSignal, (~>), filter)
import Signal.Channel (channel, subscribe, send, Channel())
import Signal.DOM

import Text.Markdown.SlamDown.Pretty
import Text.Markdown.SlamDown.Parser
import Text.Markdown.SlamDown

import Text.Smolder.HTML hiding (title)
import Text.Smolder.HTML.Attributes (id, href, className, src, lang, charset, name, content, rel, title)
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
import KeyCodes as K
import qualified UI.HTML.VDom as VDom


page404 = toHtml <<< parseMd $ "> ## 404 Not found"
initialVDom = vNode2vTree $ VDom.render $ div $ text "One moment please"
defaultTitle = getTitle appDNA --"Eugene Naumenko" -- read from appDNA

data UIState = UIState { rootNode    :: DT.Node
                       , title       :: String
                       , pageUrl     :: Url
                       , pageId      :: String
                       , cmd         :: Maybe UICmd
                       , oldVDom     :: VTree
                       , newVDom     :: VTree }

-- uiLogic :: UIActions AppState -> UIState -> UIState
uiLogic (RenderState appState) (UIState u) =
  UIState (u { oldVDom = u.newVDom
             , title   = calcTitle appState
             , pageUrl = calcPageUrl appState
             , pageId  = calcPageId appState
             , cmd     = Nothing
             , newVDom = newVDom })
  where
  newMarkup = renderHTML appState
  newVDom = vNode2vTree $ VDom.render newMarkup

uiLogic (SetCmd c apst) (UIState u) = UIState (u{cmd = Just c})

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
                                 , pageUrl: ""
                                 , pageId: ""
                                 , cmd: Nothing
                                 , newVDom: initialVDom }
    let justRender (UIState u) = case u.cmd of
                                    Nothing -> true
                                    _ -> false
    let justCmd (UIState u) = case u.cmd of
                                    Nothing -> false
                                    _ -> true

    let ui = foldp uiLogic initialUIState renderSignal
        renderSig = filter justRender initialUIState ui
        cmdSig    = filter justCmd initialUIState ui

    runSignal (patchVDom <$> renderSig)
    runSignal (resetDisqus <$> renderSig)
    runSignal (execCmd <$> ui)

    left <- keyPressed 37 -- TODO get rid of magic numbers
    right <- keyPressed 39

    runSignal $ left ~> (\x -> if x then send inputChannel $ KeyboardInput K.LeftArrow else pure unit)
    runSignal $ right ~> (\x -> if x then send inputChannel $ KeyboardInput K.RightArrow else pure unit)

    hashChanged $ \old new -> do
      log $ "hash changed: " ++ old ++ " -> " ++ new
      send inputChannel $ Navigate $ split "/" (drop 1 new)
      pure unit

    pure renderChan

execCmd (UIState u) = do
  case u.cmd of
    Just (RouteTo url) -> setLocationUrl url
    _ -> pure unit

resetDisqus :: UIState -> Eff _ Unit
resetDisqus (UIState s) = do
  baseUrl <- getBaseUrl
  resetDisqusUnsafe s.pageId (baseUrl <> s.pageUrl) s.title
  pure unit

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

    makeUrl base_url slug = "#!" <> (joinWith "/" (base_url <> [slug]))

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

    div ! id "disqus_thread" ! className "hidden" $ mempty

    script $ text (joinWith "\n" [
          ""
        -- , "var disqus_config = function () {"
        -- , "this.page.url = document.location.href;" --'" <> ("#!blog/" <> art.id) <> "';"
        -- , "this.page.identifier = document.location.hash;" --'" <> ("#!blog/" <> art.id) <> "';"
        -- , "};"
        , "(function() { "
        , "var d = document, s = d.createElement('script');"
        , "s.src = 'http://eugenen-github-io-html.disqus.com/embed.js';"
        , "s.setAttribute('data-timestamp', +new Date());"
        , "(d.head || d.body).appendChild(s);"
        , "})();"
      ])

        -- div ! className "section footer" $ do
          -- span $ text "© 2015"

  where
  menuPath = getMenuPath appState
  currentNode = getCurrentNode appState
  internalAST = fromMaybe page404 $ parseContent <$> s.currentContent
  payloadHtml = internalAST
