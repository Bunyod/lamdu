{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Common
    ( addBackground, addDarkBackground
    ) where

import           Control.Lens.Operators
import           Data.Monoid ((<>))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

addBackground :: AnimId -> Config.Layers -> Draw.Color -> Widget f -> Widget f
addBackground myId layers =
    Widget.backgroundColor (Config.layerHoleBG layers)
    (myId <> ["hole background"])

addDarkBackground :: Monad m => AnimId -> ExprGuiM m (ExpressionGui f -> ExpressionGui f)
addDarkBackground animId =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
        return $ \widget ->
            widget
            & ExpressionGui.pad (holeDarkPadding <&> realToFrac)
            & ExpressionGui.egWidget %~
              Widget.backgroundColor
              (Config.layerDarkHoleBG (Config.layers config))
              (animId <> ["hole dark background"])
              holeDarkBGColor
