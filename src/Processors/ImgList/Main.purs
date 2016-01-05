module Processors.ImgList.Main (imgListProcessor) where


import Control.Monad.Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)

import Data.Tuple
import Data.String (joinWith)
import Data.Int (fromString)

import Prelude hiding (div, map, sub)

import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes (href, className, src, lang, charset, name, content, rel)
import Text.Smolder.Markup

import Data.Foldable (for_)
import qualified Data.Array as A

import Types
import Utils
import KeyCodes

import Control.Bind ((>=>))
import Control.Monad.Aff
import Control.Monad.Aff.AVar (AVAR())
import Network.HTTP.Affjax (AJAX())


imgListProcessor :: ProcessorAPI
imgListProcessor (StringInput s) apst = go [s] apst
imgListProcessor (ArrayInput ss) apst = go ss apst

go :: forall e. Array String -> AppState -> Aff (avar :: AVAR, ajax :: AJAX | e) (Maybe Internal)
go srcs apst@(AppState s) =
  case s.currentPath of
    [] -> pure <<< Just <<< HTML <<< (renderImgList baseUrl) $ srcs

    [x] -> case fromString >=> (inRange (-1) imgsCount) $ x of
      Just n -> case s.keyboardInput of
        Nothing  -> pure <<< Just <<< HTML $ renderImgSingle baseUrl (prevUrl n) (nextUrl n) (fromJust $ srcs A.!! n)
        Just key -> pure <<< Just <<< Cmd $ (RouteTo (routeByKeyboard key n))

      _      -> pure <<< Just <<< HTML <<< errorMsg <<< noAccess $ x

    xs -> pure <<< Just <<< HTML <<< errorMsg <<< unknownRequest $ xs

    where
    inRange a b x = if x > a && x < b then Just x else Nothing

    routeByKeyboard LeftArrow  n = prevUrl n
    routeByKeyboard RightArrow n = nextUrl n

    imgsCount = A.length srcs
    baseUrl = currentPathToUrl apst
    prevUrl n = prevImgLink baseUrl imgsCount n
    nextUrl n = nextImgLink baseUrl imgsCount n
    noAccess x = "access denied for: " <> show x
    unknownRequest x = "unknown request: " <> show x

errorMsg m = div ! className "error" $ text ("Error: " <> m)
infoMsg m  = div ! className "info" $ text ("NB    : " <> m)

currentPathToUrl (AppState s) = "#" <> (fromMaybe "" (s.menuPath A.!! 0))

prevInRange a b n = if n - 1 < a then b - 1 else n - 1
nextInRange a b n = if n + 1 > b - 1 then a else n + 1

prevImgLink baseUrl imgsCount idx = imgLink baseUrl (prevInRange 0 imgsCount idx)
nextImgLink baseUrl imgsCount idx = imgLink baseUrl (nextInRange 0 imgsCount idx)

imgLink :: Url -> Int -> String
imgLink baseUrl idx = baseUrl <> "/" <> show idx

htmlLinkImg :: Url -> Tuple String Int -> Markup
htmlLinkImg baseUrl (Tuple src' idx) =
  a ! href (imgLink baseUrl idx) $
    img ! className "image" ! src src'

renderImgList :: Url -> Array String -> Markup
renderImgList baseUrl srcs =
  div ! className "img-list" $
    for_ (A.zip srcs (0 `A.range` (A.length srcs))) (htmlLinkImg baseUrl)

renderImgSingle base prev next src' = do
  div ! className "single-img" $ do
    div ! className "nav-img-close" $
      a ! href base $ text "×"

    div ! className "nav-img nav-img-left" $ do
      a ! href prev $ text "<"

    div ! className "just-img" $ do
      img ! src src'

    div ! className "nav-img nav-img-right" $ do
      a ! href next $ text ">"
