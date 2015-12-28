module Processors.ImgList.Main (imgListProcessor) where


import Control.Monad.Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class

import Data.Maybe (Maybe(..))

import Prelude



import Types
import Utils


imgListProcessor :: Input -> Aff _ (Maybe Internal)
imgListProcessor (StringInput s)   = pure $ Just $ Md $ mdImg s
imgListProcessor (ArrayInput ss)   = pure $ Just $ Md $ unlines $ mdImg <$> ss


mdImg s = "# ![" <> s <> "](" <> s <> ")"
