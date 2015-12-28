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
import Data.String.Regex
import Data.Traversable

import Network.HTTP.Affjax
import Network.HTTP.ResponseHeader
import Network.HTTP.StatusCode

import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Keys (keys)
import Data.Either

import Prelude

import Signal.Channel (send, Channel())
import Text.Markdown.SlamDown.Parser


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

data Article = Article { updatedAt :: String
                       , id        :: HexString
                       , files     :: Files }

instance isForeignArticle :: IsForeign Article where
  read raw = do
    updatedAt <- readProp "updated_at" raw
    id <- readProp "id" raw
    files <- readProp "files" raw

    pure $ Article { updatedAt: updatedAt
                   , id:        id
                   , files:     files }


blogProcessor :: Input -> Aff _ (Maybe Internal)
blogProcessor (StringInput toc) = do
  let gids = getBlogPostsIds toc
  blogPosts <- runPar $ traverse (Par <$> loadNparseGist) gids

  pure $ formatBlogPosts blogPosts

formatBlogPosts :: Array (Either ForeignError Article) -> Maybe Internal
formatBlogPosts ps = Just <<< HTML <<< toHtml <<< parseMd <<< joinWith articlesSeparator $ snippets
  where
  snippets = (render >>> take 500 >>> appendFooter) <$> ps

  render :: Either ForeignError Article -> String
  render (Left e) = show e
  render (Right art) = renderArticle art

  appendFooter s = s <> "...\n\n[Read more](?ui=html#blog/" <> "hashhash" <>")\n\n"

  articlesSeparator = "\n\n-\n\n-\n\n"

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

renderArticle :: Article -> String
renderArticle (Article a) = "> # Entry " <> a.id
                         <> "\n\n"
                         <> joinWith "\n\n***\n\n" (renderFiles a.files)

renderFiles (Files fs) = renderFile <$> fs

renderFile :: File -> String
renderFile (File f) = "> ## " <> f.name <> "\n\n" <> f.content

loadGist' :: String -> Aff _ { response :: String
                             , headers :: Array ResponseHeader
                             , status :: StatusCode }
loadGist' gid = get $ "https://api.github.com/gists/" <> gid
