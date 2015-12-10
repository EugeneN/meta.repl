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

showNode :: Maybe Node -> String
showNode Nothing = "404 No such page"
showNode (Just (Node x)) = formatText x.title x.dataSource

readSource (MemorySource x) = x

parseBody Nothing = SlamDown mempty
parseBody (Just (Node x)) = parseMd <<< readSource $ x.dataSource

class ToHtml a where
  toHtml :: a -> Markup

instance toHtmlSlamDown :: ToHtml SlamDown where
  toHtml (SlamDown bs) = toHtml bs

instance toHtmlListBlock :: ToHtml (List Block) where
  toHtml as         = p  $ do for_ as $ \x -> do toHtml x

instance toHtmlBlock :: ToHtml Block where
  toHtml (Paragraph is)         = p  $ do for_ is $ \x -> do toHtml x
  toHtml (Header n is) | n == 1 = h1 $ do for_ is $ \x -> do toHtml x
                            | n == 2 = h2 $ do for_ is $ \x -> do toHtml x
                            | n == 3 = h3 $ do for_ is $ \x -> do toHtml x
                            | n == 4 = h4 $ do for_ is $ \x -> do toHtml x
                            | n == 5 = h5 $ do for_ is $ \x -> do toHtml x
                            | n == 6 = h6 $ do for_ is $ \x -> do toHtml x
  toHtml (Blockquote bs)        = blockquote $ do for_ bs $ \x -> do toHtml x
  toHtml (Lst (Bullet _) bss)   = ul $ do for_ bss $ \x -> do toHtml x
  toHtml (Lst (Ordered _) bss)  = ol $ do for_ bss $ \x -> do toHtml x
  toHtml (CodeBlock _ ss)       = div ! className "code" $ text $ show ss
  toHtml (LinkReference l uri)  = a ! href uri $ text $ l
  toHtml Rule                   = hr

instance toHtmlInline :: ToHtml Inline where
  toHtml (Str s)                = text $ s
  toHtml (Entity s)             = text $ s
  toHtml Space                  = text $ " "
  toHtml SoftBreak              = text $ "&shy;"
  toHtml LineBreak              = br
  toHtml (Emph is)              = em $ do for_ is $ \x -> do toHtml x
  toHtml (Strong is)            = strong $ do for_ is $ \x -> do toHtml x
  toHtml (Code e s)             = div ! className "inline-code" $ text $ s
  toHtml (Link is (InlineLink uri))           = a ! href uri $ do for_ is $ \x -> do toHtml x
  toHtml (Link is (ReferenceLink Nothing))    = a ! href "#" $ do for_ is $ \x -> do toHtml x
  toHtml (Link is (ReferenceLink (Just uri))) = a ! href uri $ do for_ is $ \x -> do toHtml x
  toHtml (Image is uri)         = img ! src uri
  toHtml (FormField s b ff)     = text $ "[form-field " ++ show s ++ " " ++ show b ++ " " ++ show ff ++ "]"


formatText title (MemorySource body) =
  "\n\n" ++ (prettyPrintMd <<< parseMd) (title ++ "\n========\n\n" ++ body) ++ "\n(c) 2015\n\n"
formatText title _ =
  "\n\n" ++ (prettyPrintMd <<< parseMd) (title ++ "\n========\n\n-no data-") ++ "\n(c) 2015\n\n"

getTitle (Node x) = x.title

getMenuItems (Node x) = x.children <#> \(Node y) -> y.title

-- | Will be called on every render
-- | TODO: Implement setupUI and UI state
renderHTML :: Channel Input -> AppState -> Eff _ Unit
renderHTML inputChannel appState@(AppState s) = do
  let markdownAST = parseBody (getCurrentNode appState)
  let payloadHtml = toHtml markdownAST
  let doc = html ! lang "en" $ do
              head $ do
                meta ! charset "utf-8"
                title $ text (getTitle theSite)
                meta ! name "description" ! content "YES OMG HAI LOL"
                meta ! name "viewport" ! content "width=device-width"

                link ! rel "stylesheet" ! href "https://fonts.googleapis.com/css?family=Montserrat:400,700|Montserrat+Alternates:400,700|Montserrat+Subrayada:400,700|Quattrocento:400,700|Quattrocento+Sans:400,400italic,700,700italic&subset=latin,latin-ext"
                link ! rel "stylesheet" ! href "https://fonts.googleapis.com/css?family=Ubuntu+Mono:400,700,400italic,700italic|Source+Code+Pro:100,200,300,400,600,700,900&subset=latin,cyrillic,cyrillic-ext,greek-ext,greek,latin-ext"

                link ! rel "stylesheet" ! href "screen.css"

              body $ do
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
