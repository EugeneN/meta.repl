module UI.HTML.VDom
  ( render
  , render'
  , VNode(..)
  ) where

import Prelude hiding (div, map, sub)
import Data.Function
import qualified Data.Map as Map
import qualified Text.Smolder.Markup as Markup
-- import Text.Smolder.Markup (MarkupM(..))
import Data.Maybe
import Data.Tuple
import Data.List hiding (head, span)
import Text.Smolder.Renderer.Util (Node(..), renderMarkup)

import Text.Smolder.HTML

foreign import data VNode :: *
foreign import data VAttrs :: *

foreign import showVNodeP :: VNode -> String

instance showVNode :: Show VNode where
  show = showVNodeP

foreign import vnodeP :: Fn3 String VAttrs (Array VNode) VNode
vnode :: String -> VAttrs -> Array VNode -> VNode
vnode = runFn3 vnodeP

foreign import vtext :: String -> VNode
foreign import ventity :: String -> VNode

foreign import convertAttrsP :: Fn2 (forall k v. Map.Map k v -> Array (Tuple k v))
                                                      (Map.Map String String) VAttrs

convertAttrs :: Map.Map String String -> VAttrs
convertAttrs = (runFn2 convertAttrsP) (Map.toList >>> fromList)

render' :: forall a. Markup.MarkupM a -> Array VNode
render' m = fromList $ renderNode <$> renderMarkup m
  where renderNode (Element n a c) = vnode n (convertAttrs a) (fromList $ renderNode <$> c)
        renderNode (Text t) = vtext t
        renderNode (HtmlEntity t) = ventity t

-- The Markup type can express any number of sibling nodes.
-- Usually, you only care about the first one. This function
-- will throw a runtime error if you pass it a Markup value
-- expressing anything but a single node, returning the
-- single rendered node otherwise.
render :: forall a. Markup.MarkupM a -> VNode
render m = case render' m of [x] -> x
