{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | A hole's search term component

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm
    ( make
    ) where

import           Control.Lens.Operators
import           Control.Monad (when)
import qualified Data.Monoid as Monoid
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (addBackground)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as EventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

textEditNoEmpty :: TextEdit.Style -> TextEdit.Style
textEditNoEmpty textEditStyle =
    textEditStyle
    & TextEdit.sEmptyFocusedString .~ "  "
    & TextEdit.sEmptyUnfocusedString .~ "  "

makeSearchTermPropEdit ::
    Monad m => WidgetIds -> Property m String -> WE.WidgetEnvT m (Widget m)
makeSearchTermPropEdit WidgetIds{..} searchTermProp =
    BWidgets.makeTextEdit searchTerm hidOpenSearchTerm
    <&> Widget.events %~ \(newSearchTerm, eventRes) ->
        do
            when (newSearchTerm /= searchTerm) $
                Property.set searchTermProp newSearchTerm
            eventRes
                -- When first letter is typed in search term, jump to the
                -- results, which will go to first result:
                & ( if null searchTerm && (not . null) newSearchTerm
                    then Widget.eCursor .~ Monoid.Last (Just hidResultsPrefix)
                    else id
                  )
                & return
    where
        searchTerm = Property.value searchTermProp

make :: Monad m => HoleInfo m -> ExprGuiM m (ExpressionGui m)
make holeInfo =
    do
        config <- ExprGuiM.readConfig
        let holeConfig@Config.Hole{..} = Config.hole config
        makeSearchTermPropEdit WidgetIds{..} (HoleInfo.hiSearchTermProperty holeInfo)
            <&> Widget.eventMap %~ EventMap.disallowChars holeConfig searchTerm
            <&> addBackground (Widget.toAnimId hidOpenSearchTerm)
                (Config.layers config) holeSearchTermBGColor
            <&> ExpressionGui.fromValueWidget
            & WE.localEnv (WE.envTextStyle %~ textEditNoEmpty)
            & ExprGuiM.widgetEnv
    where
      WidgetIds{..} = hiIds holeInfo
      searchTerm = HoleInfo.hiSearchTerm holeInfo
