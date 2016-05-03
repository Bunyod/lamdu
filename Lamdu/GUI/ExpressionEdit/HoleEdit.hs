{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (addDarkBackground)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea as SearchArea
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Wrapper as Wrapper
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

makeWrapper ::
    Monad m => Sugar.Payload m ExprGuiT.Payload ->
    HoleInfo m -> ExprGuiM m (Maybe (ExpressionGui m))
makeWrapper pl holeInfo =
    hiHole holeInfo ^. Sugar.holeMArg
    & Lens._Just %%~
        ExpressionGui.wrapExprEventMap pl . Wrapper.make (hiIds holeInfo)

assignHoleCursor ::
    Monad m =>
    WidgetIds -> Maybe (Sugar.HoleArg m expr) ->
    ExprGuiM m a -> ExprGuiM m a
assignHoleCursor WidgetIds{..} Nothing =
    ExprGuiM.assignCursor hidHole hidOpen .
    ExprGuiM.assignCursor (WidgetIds.notDelegatingId hidHole) hidClosedSearchArea
assignHoleCursor WidgetIds{..} (Just _) =
    ExprGuiM.assignCursor hidHole hidWrapper .
    ExprGuiM.assignCursor (WidgetIds.notDelegatingId hidHole) hidWrapper

addSearchAreaBelow ::
    Monad m => WidgetIds ->
    ExpressionGui f -> ExpressionGui f ->
    ExprGuiM m (ExpressionGui f)
addSearchAreaBelow WidgetIds{..} wrapperGui searchAreaGui =
    do
        hoveringSearchArea <-
            searchAreaGui
            & addDarkBackground
                (Widget.toAnimId hidOpen ++ ["searchAreaDarkBg"])
            >>= ExpressionGui.liftLayers
        wrapperGui
            & Layout.addAfter Layout.Vertical [hoveringSearchArea]
            & return

addWrapperAbove ::
    Monad m =>
    WidgetIds -> ExpressionGui f -> ExpressionGui f ->
    ExprGuiM m (ExpressionGui f)
addWrapperAbove WidgetIds{..} wrapperGui searchAreaGui =
    do
        Config.Hole{..} <- ExprGuiM.readConfig <&> Config.hole
        hoveringWrapper <-
            addDarkBackground (Widget.toAnimId hidWrapper ++ ["wrapperDarkBg"])
            wrapperGui
            <&> Layout.scale (holeHoveringWrapperScaleFactor <&> realToFrac)
            >>= ExpressionGui.liftLayers
        searchAreaGui
            & Layout.addBefore Layout.Vertical [hoveringWrapper]
            & return

make ::
    Monad m =>
    Sugar.Hole (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make hole pl =
    do
        Config.Hole{..} <- ExprGuiM.readConfig <&> Config.hole

        stateProp <-
            HoleState.assocStateRef (hole ^. Sugar.holeActions . Sugar.holeUUID)
            ^. Transaction.mkProperty & ExprGuiM.transaction

        let holeInfo = HoleInfo
                { hiEntityId = pl ^. Sugar.plEntityId
                , hiState = stateProp
                , hiInferredType = pl ^. Sugar.plAnnotation . Sugar.aInferredType
                , hiHole = hole
                , hiIds = WidgetIds{..}
                , hiNearestHoles = pl ^. Sugar.plData . ExprGuiT.plNearestHoles
                }

        do
            mWrapperGui <- makeWrapper pl holeInfo
            case mWrapperGui of
                Just wrapperGui ->
                    do
                        unfocusedWrapperGui <-
                            wrapperGui & ExpressionGui.maybeAddAnnotationPl pl
                        isSelected <- ExprGuiM.widgetEnv $ WE.isSubCursor hidHole
                        let layout f = do
                                searchAreaGui <-
                                    SearchArea.makeStdWrapped pl holeInfo
                                f WidgetIds{..} wrapperGui searchAreaGui
                                    <&> (`Layout.hoverInPlaceOf` unfocusedWrapperGui)
                        if Widget.isFocused (wrapperGui ^. ExpressionGui.egWidget)
                            then layout addSearchAreaBelow
                            else if isSelected then
                                     layout addWrapperAbove
                                 else
                                     return unfocusedWrapperGui
                Nothing -> SearchArea.makeStdWrapped pl holeInfo
            & assignHoleCursor WidgetIds{..} (hole ^. Sugar.holeMArg)
    where
        WidgetIds{..} = HoleWidgetIds.make (pl ^. Sugar.plEntityId)
