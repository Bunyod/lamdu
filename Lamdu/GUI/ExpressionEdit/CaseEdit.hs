{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.CaseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.Type (Tag)
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

type T = Transaction

destCursorId ::
    [Sugar.CaseAlt name n (Sugar.Expression name n p)] ->
    Widget.Id -> Widget.Id
destCursorId [] defDestId = defDestId
destCursorId (alt : _) _ =
    alt ^. Sugar.caHandler . Sugar.rPayload & WidgetIds.fromExprPayload

make ::
    Monad m =>
    Sugar.Case (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Case mArg alts caseTail addAlt cEntityId) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    let headerId = Widget.joinId myId ["header"]
    in ExprGuiM.assignCursor myId (destCursorId alts headerId) $
    do
        config <- ExprGuiM.readConfig
        let mExprAfterHeader =
                ( alts ^.. Lens.traversed . Lens.traversed
                ++ caseTail ^.. Lens.traversed
                ) ^? Lens.traversed
        labelJumpHoleEventMap <-
            mExprAfterHeader <&> ExprGuiT.nextHolesBefore
            & Lens._Just ExprEventMap.jumpHolesEventMap
            <&> fromMaybe mempty
        let headerLabel text =
                WidgetIds.fromEntityId cEntityId
                & Widget.toAnimId
                & ExpressionGui.grammarLabel text
                >>= ExpressionGui.makeFocusableView headerId
                <&> ExpressionGui.egWidget
                    %~ Widget.weakerEvents labelJumpHoleEventMap
        (mActiveTag, header) <-
            case mArg of
            Sugar.LambdaCase -> headerLabel "λ:" <&> (,) Nothing
            Sugar.CaseWithArg (Sugar.CaseArg arg toLambdaCase) ->
                do
                    argEdit <-
                        ExprGuiM.makeSubexpression (const 0) arg
                        <&> ExpressionGui.egWidget %~ Widget.weakerEvents
                            (toLambdaCaseEventMap config toLambdaCase)
                    caseLabel <- headerLabel ":"
                    mTag <-
                        ExpressionGui.evaluationResult (arg ^. Sugar.rPayload)
                        <&> (>>= (^? ER.body . ER._RInject . V.injectTag))
                    return (mTag, ExpressionGui.hbox [argEdit, caseLabel])
        (altsGui, resultPickers) <-
            ExprGuiM.listenResultPickers $
            do
                altsGui <- makeAltsWidget mActiveTag alts myId
                case caseTail of
                    Sugar.ClosedCase deleteTail ->
                        altsGui
                        & ExpressionGui.egWidget %~
                          Widget.weakerEvents
                          (caseOpenEventMap config deleteTail)
                        & return
                    Sugar.CaseExtending rest ->
                        altsGui
                        & makeOpenCase rest (Widget.toAnimId myId)
        let addAltEventMap =
                ExprGuiM.holePickersAction resultPickers >> addAlt
                <&> (^. Sugar.caarNewTag . Sugar.tagInstance)
                <&> WidgetIds.fromEntityId
                <&> TagEdit.diveToCaseTag
                & Widget.keysEventMapMovesCursor (Config.caseAddAltKeys config)
                  (E.Doc ["Edit", "Case", "Add Alt"])
        vspace <- ExpressionGui.verticalSpace
        [header, vspace, altsGui]
            & ExpressionGui.vboxTopFocalAlignedTo 0
            & ExpressionGui.addValFrame myId
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents addAltEventMap

makeAltRow ::
    Monad m =>
    Maybe Tag ->
    Sugar.CaseAlt (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload) ->
    ExprGuiM m [ExpressionGui m]
makeAltRow mActiveTag (Sugar.CaseAlt delete tag altExpr) =
    do
        config <- ExprGuiM.readConfig
        altRefGui <-
            TagEdit.makeCaseTag (ExprGuiT.nextHolesBefore altExpr) tag
            >>= if mActiveTag == Just (tag ^. Sugar.tagVal)
                then
                    ExpressionGui.egWidget %%~
                    ExpressionGui.addValBGWithColor Config.evaluatedPathBGColor
                    (WidgetIds.fromEntityId (tag ^. Sugar.tagInstance))
                else return
        altExprGui <- ExprGuiM.makeSubexpression (const 0) altExpr
        let itemEventMap = caseDelEventMap config delete
        space <- ExpressionGui.stdSpace
        [ altRefGui & ExpressionGui.egAlignment . _1 .~ 1
            , space
            , altExprGui & ExpressionGui.egAlignment . _1 .~ 0
            ]
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents itemEventMap
            & return

makeAltsWidget ::
    Monad m =>
    Maybe Tag ->
    [Sugar.CaseAlt (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload)] ->
    Widget.Id -> ExprGuiM m (ExpressionGui m)
makeAltsWidget _ [] myId =
    ExpressionGui.grammarLabel "Ø" (Widget.toAnimId myId)
    >>= ExpressionGui.makeFocusableView (Widget.joinId myId ["Ø"])
makeAltsWidget mActiveTag alts _ =
    do
        vspace <- ExpressionGui.verticalSpace
        mapM (makeAltRow mActiveTag) alts
            <&> List.intersperse (replicate 3 vspace)
            <&> ExpressionGui.gridTopLeftFocal

separationBar :: Config -> Widget.R -> Anim.AnimId -> ExpressionGui m
separationBar config width animId =
    Anim.unitSquare (animId <> ["tailsep"])
    & View 1
    & Widget.fromView
    & Widget.tint (Config.caseTailColor config)
    & Widget.scale (Vector2 width 10)
    & ExpressionGui.fromValueWidget

makeOpenCase ::
    Monad m =>
    ExprGuiT.SugarExpr m -> AnimId -> ExpressionGui m ->
    ExprGuiM m (ExpressionGui m)
makeOpenCase rest animId altsGui =
    do
        config <- ExprGuiM.readConfig
        vspace <- ExpressionGui.verticalSpace
        restExpr <-
            ExprGuiM.makeSubexpression (const 0) rest
            >>= ExpressionGui.addValPadding
        let minWidth = restExpr ^. ExpressionGui.egWidget . Widget.width
        [ altsGui
            , separationBar config (max minWidth targetWidth) animId
            , vspace
            , restExpr
            ] & ExpressionGui.vboxTopFocalAlignedTo 0 & return
    where
        targetWidth = altsGui ^. ExpressionGui.egWidget . Widget.width

caseOpenEventMap ::
    Monad m =>
    Config -> T m Sugar.EntityId -> Widget.EventHandlers (T m)
caseOpenEventMap config open =
    Widget.keysEventMapMovesCursor (Config.caseOpenKeys config)
    (E.Doc ["Edit", "Case", "Open"]) $ WidgetIds.fromEntityId <$> open

caseDelEventMap ::
    Monad m =>
    Config -> T m Sugar.EntityId -> Widget.EventHandlers (T m)
caseDelEventMap config delete =
    Widget.keysEventMapMovesCursor (Config.delKeys config)
    (E.Doc ["Edit", "Case", "Delete Alt"]) $ WidgetIds.fromEntityId <$> delete

toLambdaCaseEventMap ::
    Monad m =>
    Config -> T m Sugar.EntityId -> Widget.EventHandlers (T m)
toLambdaCaseEventMap config toLamCase =
    Widget.keysEventMapMovesCursor (Config.delKeys config)
    (E.Doc ["Edit", "Case", "Turn to Lambda-Case"]) $
    WidgetIds.fromEntityId <$> toLamCase
