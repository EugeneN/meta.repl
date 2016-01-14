module Processors.Blog.Main (blogProcessor) where


import Control.Monad.Aff
import Control.Monad.Aff.Par
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class

import Control.Alt(Alt, (<|>))
import Control.Monad.Eff.Exception(error)
import Control.Monad.Error.Class(throwError)

import qualified Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith, drop, take)
import Data.String.Regex hiding (source)
import Data.Traversable

import Network.HTTP.Affjax
import Network.HTTP.ResponseHeader
import Network.HTTP.StatusCode

import Data.Foreign
import Data.Foreign.Index (Index)
import Data.Foreign.Class
import Data.Foreign.Keys (keys)
import Data.Either

import Prelude hiding (div, map, sub, id)

import Signal.Channel (send, Channel())
import Text.Markdown.SlamDown.Parser

import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes (id, href, className, src, lang, charset, name, content, rel)
import Text.Smolder.Markup

import Data.Foldable (for_)
import Data.Monoid (mempty)

import Data.Date
import Data.Date.UTC

import Types
import Utils
import Internal

data Error = Error String

instance showError :: Show Error where
  show (Error e) = e

data Mimetype = Plaintext | UnknownMimetype String

instance isForeignMymetype :: IsForeign Mimetype where
  read raw = do
    x <- read raw
    pure $ case x of
      "text/plain" -> Plaintext
      _            -> UnknownMimetype x

type HexString = String

data Files = Files (Array File)

instance isForeignFiles :: IsForeign Files where
  read raw = do
    ks <- keys raw
    files <- for ks $ \k -> do
      file <- readProp k raw
      pure file
    pure $ Files files

data File = File { content  :: String
                 , name     :: String
                 , lang     :: String
                 , size     :: Int
                 , mimetype :: Mimetype }

instance isForeignFile :: IsForeign File where
  read raw = do
    c <- readProp "content"  raw
    n <- readProp "filename" raw
    l <- readProp "language" raw
    s <- readProp "size"     raw
    m <- readProp "type"     raw
    pure $ File { content:  c
                , name:     n
                , lang:     l
                , size:     s
                , mimetype: m }


data Article = Article { updatedAt   :: Maybe Date
                       , createdAt   :: Maybe Date
                       , id          :: HexString
                       , description :: String
                       , files       :: Files }

readDate :: forall k. (Index k) => k -> Foreign -> F (Maybe Date)
readDate key obj = case readProp key obj of
  Right y -> Right $ fromString y
  _       -> Right Nothing

instance isForeignArticle :: IsForeign Article where
  read raw = do
    updatedAt <- readDate "updated_at" raw
    createdAt <- readDate "created_at" raw
    id        <- readProp "id" raw
    desc      <- readProp "description" raw
    files     <- readProp "files" raw

    pure $ Article { updatedAt: updatedAt
                   , createdAt: createdAt
                   , id:        id
                   , description: desc
                   , files:     files }

blogProcessor :: ProcessorAPI
blogProcessor (StringInput toc) apst@(AppState s) = do
  case s.currentPath of
    [] -> do
      let gids = getBlogPostsIds toc
      blogPosts <- runPar $ traverse (Par <$> loadNparseGist) gids

      pure $ renderIndex blogPosts apst

    [x] -> do
      -- basic security check
      let gids = getBlogPostsIds toc
      case A.elemIndex x gids of
        Just _ -> do
          cont <- loadNparseGist x
          pure $ Just <<< HTML $ either (errorMsg <<< show) renderFullArticle cont

        _ -> pure $ Just <<< HTML <<< errorMsg $ ("access denied for id: " <> show x)

    _ -> pure $ Just <<< HTML <<< errorMsg $ ("unknown request: " <> show s.currentPath)

-- TODO lib
errorMsg m = div ! className "error" $ text ("Error: " <> m)
infoMsg m  = div ! className "info" $ text ("NB    : " <> m)

renderIndex :: Array (Either Error Article) -> AppState -> Maybe Internal
renderIndex ps apst = Just <<< HTML <<< renderListH $ ps
  where

  renderListH :: Array (Either Error Article) -> Markup
  renderListH ps = do
    ul ! className "articles-list" $ do
      for_ ps (either (errorMsg <<< show) renderArticleIndexTitle)

renderFullArticle :: Article -> Markup
renderFullArticle (Article art) = do
  div ! className "sub-nav" $ do
    a ! href "#!blog" $ text "↑up to index"
  div ! className "article" $ do
    div ! className "article-file-body" $ do
      renderFilesH art.files

    div ! className "comments-block" $ do
      div ! id "disqus_thread" $ mempty

showDate :: Maybe Date -> String
showDate (Just d) = (take 3 $ show $ month d) <> " " <> (showDay $ dayOfMonth d) <> ", " <> (showYear $ year d)
  where
    showDay (DayOfMonth n) = show n
    showYear (Year n) = show n

showDate _        = "Invalid date"


renderArticleIndexTitle :: Article -> Markup
renderArticleIndexTitle (Article art) =
  li ! className "articles-index" $ do
    div ! className "article-index-title" $ do
      span $ text $ showDate art.createdAt
      span $ text "⇒"
      a ! href ("#!blog/" <> art.id) $ toHtml <<< parseMd $ art.description

renderFilesH :: Files -> Markup
renderFilesH (Files fs) = for_ fs renderFileH

renderFileH :: File -> Markup
renderFileH (File f) =
  div ! className "article-file" $ do
    -- h2 ! className "article-file-name" $ text f.name
    div ! className "article-file-body" $ do
      toHtml <<< parseMd $ f.content

getBlogPostsIds toc = cleanIds
  where
  regexFlags = noFlags{ global = true, ignoreCase = true, multiline = true }
  idRegex    = regex "\\([a-f0-9]{20}\\)" regexFlags
  rawIds     = match idRegex toc
  justIds    = fromMaybe [] (A.catMaybes <$> rawIds)
  cleanIds   = (drop 1 >>> take 20) <$> justIds

loadNparseGist gid = do
  g <- loadGist' gid
  case g.status of
    StatusCode 200 -> pure $ parseJsonGistResponse g.response
    StatusCode x   -> pure <<< Left <<< Error $ ("Bad response: " <> show x)


parseJsonGistResponse :: String -> Either Error Article
parseJsonGistResponse respJson = case readJSON respJson :: F Article of
  Left e  -> Left <<< Error <<< show $ e
  Right x -> Right x

loadGist' :: String -> Aff _ { response :: String
                             , headers  :: Array ResponseHeader
                             , status   :: StatusCode }
loadGist' gid = get $ "https://api.github.com/gists/" <> gid
