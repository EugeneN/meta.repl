module UI.HTML.Main (renderHTML) where

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
-- import qualified Text.Smolder.Renderer.Util as SU -- (SU.renderMarkup, Node())

import Data
import Types
import Core

foreign import setInnerHTML :: forall e. String -> Eff e Unit
foreign import toString :: forall a. a -> String

readSource (MemorySource x) = x

parseBody Nothing = SlamDown mempty
parseBody (Just (Node x)) = parseMd <<< readSource $ x.dataSource

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


getTitle (Node x) = x.title
getMenuItems (Node x) = x.children <#> \(Node y) -> y.title

-- | Will be called on every render
-- | TODO: Implement setupUI and UI state
renderHTML :: Channel Input -> AppState -> Eff _ Unit
renderHTML inputChannel appState@(AppState s) = do
  let markdownAST = parseBody (getCurrentNode appState)
  let payloadHtml = toHtml markdownAST
  let doc = body $ do

                link ! rel "stylesheet" ! href "https://fonts.googleapis.com/css?family=Montserrat:400,700|Montserrat+Alternates:400,700|Montserrat+Subrayada:400,700|Quattrocento:400,700|Quattrocento+Sans:400,400italic,700,700italic&subset=latin,latin-ext"
                link ! rel "stylesheet" ! href "https://fonts.googleapis.com/css?family=Ubuntu+Mono:400,700,400italic,700italic|Source+Code+Pro:100,200,300,400,600,700,900&subset=latin,cyrillic,cyrillic-ext,greek-ext,greek,latin-ext"

                link ! rel "stylesheet" ! href "screen.css"

                div ! className "content" $ do
                  div ! className "section" $ do
                    a ! href "?ui=console" $ text "Console UI"
                    h1 ! className "name" $ text (getTitle theSite)

                    div ! className "nav" $ do
                      for_ (getMenuItems theSite) $ \x -> do
                        a ! href ("#" ++ x) $ text x

                    div ! className "section page"  $ do
                      h2 $ text $ fromMaybe "-no title-" (getTitle <$> getCurrentNode appState)
                      div ! className "text" $ do
                        payloadHtml

                      div ! className "section footer" $ do
                        span $ text "&copy; 2015"

  log $ show markdownAST
  log $ toString $ payloadHtml
  setInnerHTML $ render doc
