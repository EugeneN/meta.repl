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

data Article = Article { updatedAt   :: String
                       , id          :: HexString
                       , description :: String
                       , files       :: Files }

instance isForeignArticle :: IsForeign Article where
  read raw = do
    updatedAt <- readProp "updated_at" raw
    id <- readProp "id" raw
    desc <- readProp "description" raw
    files <- readProp "files" raw

    pure $ Article { updatedAt: updatedAt
                   , id:        id
                   , description: desc
                   , files:     files }

blogProcessor :: ProcessorAPI
blogProcessor (StringInput toc) apst@(AppState s) = do
  case s.currentPath of
    [] -> do
      let gids = getBlogPostsIds toc
      blogPosts <- runPar $ traverse (Par <$> loadNparseGist) gids

      pure $ formatBlogPosts blogPosts apst

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

formatBlogPosts :: Array (Either Error Article) -> AppState -> Maybe Internal
formatBlogPosts ps apst = Just <<< HTML <<< renderListH $ ps
  where

  renderListH :: Array (Either Error Article) -> Markup
  renderListH ps = do
    div ! className "articles-list" $ do
      for_ ps (either (errorMsg <<< show) renderArticleH)

renderFullArticle :: Article -> Markup
renderFullArticle (Article art) = do
  div ! className "sub-nav" $ do
    a ! href "#blog" $ text "↑up to index"
  div ! className "article" $ do
    div ! className "article-file-body" $ do
      toHtml <<< parseMd $ art.description
      renderFilesH art.files

    div ! className "comments-block" $ do
      div ! id "disqus_thread" $ mempty
      script $ text (joinWith "\n" [
            "var disqus_config = function () {"
          , "//this.page.url = '" <> ("#blog/" <> art.id) <> "';"
          , "this.page.identifier = '" <> ("#blog/" <> art.id) <> "';"
          , "};"
          , "(function() { "
          , "var d = document, s = d.createElement('script');"
          , "s.src = '//eugenen-github-io-html.disqus.com/embed.js';"
          , "s.setAttribute('data-timestamp', +new Date());"
          , "(d.head || d.body).appendChild(s);"
          , "})();"
        ])

renderArticleH :: Article -> Markup
renderArticleH (Article art) =
  div ! className "article" $ do
    -- div ! className "article-title" $ do
      -- a ! href ("?ui=html#blog/" <> art.id) $ text $ "Entry: " <> art.id
    div ! className "article-file-body" $ do
      span $ text "Entry "
      a ! href ("#blog/" <> art.id) $ text art.id
      -- a ! href ("https://eugenen.github.io/C.MD/#!" <> art.id <> ";p") $ text art.id
      span $ text ": "

      toHtml <<< parseMd $ art.description
    -- renderFilesH art.files

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
