module Main where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff())

import Data.Maybe

import Signal (foldp, runSignal, (~>))
import Signal.Channel (channel, subscribe)

import Types
import Data
import Core
import Utils
import UI.Console.Main (setupCliUi)
import UI.HTML.Main    (setupHtmlUi)
import UI.Telnet.Main  (setupTelnetUi)


data UiParam = HtmlUI | ConsoleUI | TelnetUI

parseUiParam = do
  x <- getParameterByName' "ui"
  pure $ case x of
    Just "html"    -> Just HtmlUI
    Just "console" -> Just ConsoleUI
    Just "telnet"  -> Just TelnetUI
    _              -> Nothing

parseCommentsParam = do
  x <- getParameterByName' "comments"
  pure $ case x of
    Just "disqus"      -> Just Disqus
    Just "livefyre"    -> Just Livefyre
    Just "nocomments"  -> Just NoComments
    _                  -> Nothing

setupUI appSignal actionsChannel uiParam = do
  uiChannel <- case uiParam of
    HtmlUI    -> setupHtmlUi   actionsChannel
    ConsoleUI -> setupCliUi    actionsChannel
    TelnetUI  -> setupTelnetUi actionsChannel

  runSignal (appSignal ~> (appEffectsLogic uiChannel))

setCommentsMode (AppState s) m = AppState s{commentsMode = m}

main = do
  platform <- platformDetect'
  log $ "Platform " ++ show platform

  uiParam <- case platform of
    Browser -> do
      x <- parseUiParam
      pure $ Just $ fromMaybe ConsoleUI x

    Nodejs  -> pure $ Just TelnetUI
    _       -> pure Nothing

  commentsParam <- case platform of
    Browser -> do
      y <- parseCommentsParam
      pure $ fromMaybe Disqus y

    _ -> pure NoComments

  let initialState' = setCommentsMode initialState commentsParam

  log $ "Setting comments mode " ++ show commentsParam

  actionsChannel <- channel Noop

  let actionsSignal = subscribe actionsChannel
  let appSignal = foldp appLogic initialState' actionsSignal

  case uiParam of
    Just x     -> setupUI appSignal actionsChannel x
    Nothing    -> log "Error: No interface."
