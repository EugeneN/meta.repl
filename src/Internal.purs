module Internal where

import Prelude hiding (div, map, sub)

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())

import Data.Foldable (for_)
import Data.List hiding (head, span)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)

import Text.Markdown.SlamDown

import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes (href, className, src, lang, charset, name, content, rel)
import Text.Smolder.Markup

import VirtualDOM.VTree

import Types
import Utils


class ToHtml a where
  toHtml :: a -> Markup

instance toHtmlSlamDown :: ToHtml SlamDown where
  toHtml (SlamDown bs) = toHtml bs

instance toHtmlListBlock :: ToHtml (List Block) where
  toHtml as = span  $ do for_ as toHtml

instance toHtmlBlock :: ToHtml Block where
  toHtml (Paragraph is)              = p  $ for_ is toHtml
  toHtml (Header n is) | n == 1      = h1 $ for_ is toHtml
                            | n == 2 = h2 $ for_ is toHtml
                            | n == 3 = h3 $ for_ is toHtml
                            | n == 4 = h4 $ for_ is toHtml
                            | n == 5 = h5 $ for_ is toHtml
                            | n == 6 = h6 $ for_ is toHtml
  toHtml (Blockquote bs)             = blockquote $ for_ bs toHtml
  toHtml (Lst (Bullet _) bss)        = ul $ for_ bss (\bs -> li $ for_ bs toHtml)
  toHtml (Lst (Ordered _) bss)       = ol $ for_ bss (\bs -> li $ for_ bs toHtml)
  toHtml (CodeBlock _ ss)            = pre ! className "code" $ for_ ss (\s -> code ! className "code-line language-haskell" $ text s)
  toHtml (LinkReference l uri)       = a ! href uri $ text $ l
  toHtml Rule                        = hr

instance toHtmlInline :: ToHtml Inline where
  toHtml (Str s)                     = text $ s
  toHtml (Entity s)                  = htmlEntity $ s
  toHtml Space                       = text $ " "
  toHtml SoftBreak                   = text $ " " --"&shy;"
  toHtml LineBreak                   = br
  toHtml (Emph is)                   = em     $ for_ is toHtml
  toHtml (Strong is)                 = strong $ for_ is toHtml
  toHtml (Code e s)                  = code ! className "inline-code language-haskell" $ text $ s
  toHtml (Link is (InlineLink uri))           = a ! href uri $ for_ is toHtml
  toHtml (Link is (ReferenceLink Nothing))    = a ! href "#!" $ for_ is toHtml
  toHtml (Link is (ReferenceLink (Just uri))) = a ! href uri $ for_ is toHtml
  toHtml (Image is uri)              = img ! src uri
  toHtml (FormField s b ff)          = text $ "[form-field " ++ show s ++ " " ++ show b ++ " " ++ show ff ++ "]"
