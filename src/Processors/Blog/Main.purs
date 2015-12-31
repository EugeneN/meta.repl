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

import Prelude hiding (div, map, sub)

import Signal.Channel (send, Channel())
import Text.Markdown.SlamDown.Parser

import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes (href, className, src, lang, charset, name, content, rel)
import Text.Smolder.Markup

import Data.Foldable (for_)

import Types
import Utils
import Internal

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
      cont <- loadNparseGist x
      pure $ Just <<< HTML $ either (errorMsg <<< show) renderFullArticle cont

    _ -> pure $ Just <<< HTML <<< errorMsg $ ("unknown request: " <> show s.currentPath)

errorMsg m = div ! className "error" $ text ("Error: " <> m)
infoMsg m = div ! className "info" $ text ("NB: " <> m)

formatBlogPosts :: Array (Either ForeignError Article) -> AppState -> Maybe Internal
formatBlogPosts ps apst = Just <<< HTML <<< renderListH $ ps
  where

  renderListH :: Array (Either ForeignError Article) -> Markup
  renderListH ps = do
    div ! className "blog-note" $ blogNote
    hr
    div ! className "articles-list" $ do
      for_ ps (either (errorMsg <<< show) renderArticleH)

  blogNote = toHtml <<< parseMd <<< unlines $
    [ "*NB*: Posts for this blog are written in [C.MD gist editor](http://eugenen.github.io/C.MD) "
    , "and persisted in [Github Gists](https://gist.github.com/). "
    , ""
    , ""
    , "Thus, the blog is a symbiosis between 2 *pure clientside* "
    , "applications and 3rd-party API/service via CORS. There is no «classical» backend, "
    , "and no databases were harmed while making this blog :-)"
    ]

renderFullArticle :: Article -> Markup
renderFullArticle (Article art) = do
  div ! className "sub-nav" $ do
    a ! href "#blog" $ text "↑up to index"
  div ! className "article" $ do
    div ! className "article-file-body" $ do
      toHtml <<< parseMd $ art.description
      renderFilesH art.files

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
  regexFlags = noFlags{ global= true, ignoreCase= true, multiline= true }
  idRegex    = regex "\\([a-f0-9]{20}\\)" regexFlags
  rawIds     = match idRegex toc
  justIds    = fromMaybe [] (A.catMaybes <$> rawIds)
  cleanIds   = (drop 1 >>> take 20) <$> justIds

loadNparseGist gid = do
  g <- loadGist' gid
  pure $ parseJsonGistResponse g.response

parseJsonGistResponse :: String -> Either ForeignError Article
parseJsonGistResponse respJson = readJSON respJson :: F Article

loadGist' :: String -> Aff _ { response :: String
                             , headers :: Array ResponseHeader
                             , status :: StatusCode }
loadGist' gid = get $ "https://api.github.com/gists/" <> gid
