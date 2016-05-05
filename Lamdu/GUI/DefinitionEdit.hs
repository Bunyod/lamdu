{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit
    ( make
    ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import           Lamdu.Calc.Type.Scheme (Scheme(..), schemeType)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..), DefinitionN)
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

type T = Transaction

make ::
    Monad m =>
    DefinitionN m ExprGuiT.Payload ->
    ExprGuiM m (Widget (T m Widget.EventResult))
make exprGuiDefS =
    case exprGuiDefS ^. Sugar.drBody of
    Sugar.DefinitionBodyExpression bodyExpr ->
        makeExprDefinition exprGuiDefS bodyExpr
    Sugar.DefinitionBodyBuiltin builtin ->
        makeBuiltinDefinition exprGuiDefS builtin
    <&> (^. ExpressionGui.egWidget)

expandTo :: Widget.R -> ExpressionGui m -> ExpressionGui m
expandTo width eg
    | padding <= 0 = eg
    | otherwise = eg & ExpressionGui.pad (Vector2 (padding / 2) 0)
    where
        padding = width - eg ^. ExpressionGui.egWidget . Widget.width

topLevelSchemeTypeView ::
    Monad m => Widget.R -> Scheme -> Sugar.EntityId -> AnimId -> ExprGuiM m (ExpressionGui m)
topLevelSchemeTypeView width scheme entityId suffix =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    WidgetIds.fromEntityId entityId
    & (`Widget.joinId` suffix)
    & Widget.toAnimId
    & ExpressionGui.makeTypeView (scheme ^. schemeType)
    <&> expandTo width

makeBuiltinDefinition ::
    Monad m =>
    Sugar.Definition (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionBuiltin m -> ExprGuiM m (ExpressionGui m)
makeBuiltinDefinition def builtin =
    do
        assignment <-
            [ ExpressionGui.makeNameOriginEdit name (Widget.joinId myId ["name"])
            , ExprGuiM.makeLabel "=" $ Widget.toAnimId myId
            , BuiltinEdit.make builtin myId
            ]
            & sequenceA
            >>= ExprGuiM.widgetEnv . BWidgets.hboxCenteredSpaced
            <&> ExpressionGui.fromValueWidget
        let width = assignment ^. ExpressionGui.egWidget . Widget.width
        typeView <-
            topLevelSchemeTypeView width (builtin ^. Sugar.biType) entityId ["builtinType"]
        [assignment, typeView]
            & ExpressionGui.vboxTopFocalAlignedTo 0
            & return
    where
        name = def ^. Sugar.drName
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId

typeIndicatorId :: Widget.Id -> Widget.Id
typeIndicatorId myId = Widget.joinId myId ["type indicator"]

typeIndicator ::
    Monad m => Widget.R -> Draw.Color -> Widget.Id -> ExprGuiM m (ExpressionGui m)
typeIndicator width color myId =
    do
        config <- ExprGuiM.readConfig
        let typeIndicatorHeight =
                realToFrac $ Config.typeIndicatorFrameWidth config ^. _2
        Anim.unitSquare (Widget.toAnimId (typeIndicatorId myId))
            & View 1
            & Widget.fromView
            & Widget.scale (Vector2 width typeIndicatorHeight)
            & Widget.tint color
            & ExpressionGui.fromValueWidget
            & return

acceptableTypeIndicator ::
    Monad m =>
    Widget.R -> T m a -> Draw.Color -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
acceptableTypeIndicator width accept color myId =
    do
        config <- ExprGuiM.readConfig
        let acceptKeyMap =
                Widget.keysEventMapMovesCursor (Config.acceptDefinitionTypeKeys config)
                (E.Doc ["Edit", "Accept inferred type"]) (accept >> return myId)
        typeIndicator width color myId
            >>= ExpressionGui.makeFocusableView (typeIndicatorId myId)
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents acceptKeyMap

makeExprDefinition ::
    Monad m =>
    Sugar.Definition (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionExpression (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeExprDefinition def bodyExpr =
    do
        config <- ExprGuiM.readConfig
        bodyGui <-
            BinderEdit.make (def ^. Sugar.drName)
            (bodyExpr ^. Sugar.deContent) myId
        let width = bodyGui ^. ExpressionGui.egWidget . Widget.width
        vspace <- ExpressionGui.stdVSpace
        typeGui <-
            case bodyExpr ^. Sugar.deTypeInfo of
            Sugar.DefinitionExportedTypeInfo scheme ->
                typeIndicator width (Config.typeIndicatorMatchColor config) myId :
                [ topLevelSchemeTypeView width scheme entityId ["exportedType"]
                ] & sequence
            Sugar.DefinitionNewType (Sugar.AcceptNewType oldMExported newInferred accept) ->
                case oldMExported of
                Definition.NoExportedType ->
                    [ topLevelSchemeTypeView width newInferred entityId ["inferredType"]
                    , acceptableTypeIndicator width accept (Config.typeIndicatorFirstTimeColor config) myId
                    ]
                Definition.ExportedType oldExported ->
                    [ topLevelSchemeTypeView width newInferred entityId ["inferredType"]
                    , acceptableTypeIndicator width accept (Config.typeIndicatorErrorColor config) myId
                    , topLevelSchemeTypeView width oldExported entityId ["exportedType"]
                    ]
                & sequence
            <&> ExpressionGui.vboxTopFocalAlignedTo 0 . concatMap (\w -> [vspace, w])
        ExpressionGui.vboxTopFocalAlignedTo 0 [bodyGui, typeGui]
            & return
    where
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId
