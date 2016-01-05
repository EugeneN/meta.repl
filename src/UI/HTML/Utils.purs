module UI.HTML.Utils where

import Prelude hiding (div, map, sub)

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())

import qualified DOM.Node.Types as DT
import Text.Markdown.SlamDown.Parser
import Text.Smolder.Markup

import VirtualDOM.VTree

import Types
import Utils
import Core
import Internal
import qualified UI.HTML.VDom as VDom
import Unsafe.Coerce (unsafeCoerce)

foreign import setLocationUrl :: forall e. Url -> Eff e Unit

parseContent :: Internal -> Markup
parseContent (Md x) = toHtml $ parseMd x
parseContent (HTML h) = h

vNode2vTree :: VDom.VNode -> VTree
vNode2vTree = unsafeCoerce

foreign import appendToBody :: forall e. DT.Node -> Eff e Unit

data MenuItem = MenuItem Url String
getMenuItems (Node x) = x.children <#> \(Node y) -> MenuItem y.path y.title
