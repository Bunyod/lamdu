{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.ExpressionEdit.EventMap
    ( make
    , modifyEventMap
    , jumpHolesEventMap
    , extractCursor
    ) where

import           Prelude.Compat

import           Control.Applicative (liftA2)
import           Control.Lens.Operators
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (EventHandlers)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.CharClassification (operatorChars)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleEditState
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, HolePickers)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

data IsHoleResult = HoleResult | NotHoleResult

make ::
    Monad m => HolePickers m -> Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (EventHandlers (T m))
make holePickers pl =
    mconcat <$> sequenceA
    [ actionsEventMap holePickers isHoleResult (pl ^. Sugar.plActions)
    , jumpHolesEventMapIfSelected pl
    , replaceOrComeToParentEventMap pl
    ]
    where
        isHoleResult
            | ExprGuiT.plOfHoleResult pl = HoleResult
            | otherwise = NotHoleResult

mkEventMapWithPickers ::
    (Functor f, Monad m) =>
    HolePickers m ->
    [ModKey] -> E.Doc ->
    (f Sugar.EntityId -> T m Widget.Id) ->
    f Sugar.EntityId -> EventHandlers (T m)
mkEventMapWithPickers holePickers keys doc f =
    E.keyPresses keys doc .
    liftA2 mappend (ExprGuiM.holePickersAction holePickers) .
    fmap Widget.eventResultFromCursor . f

jumpHolesEventMap :: Monad m => NearestHoles -> ExprGuiM m (EventHandlers (T m))
jumpHolesEventMap hg =
    do
        config <- ExprGuiM.readConfig <&> Config.hole
        let jumpEventMap keys dirStr lens =
                maybe mempty
                (Widget.keysEventMapMovesCursor (keys config)
                  (E.Doc ["Navigation", jumpDoc dirStr]) . pure . WidgetIds.fromEntityId) $
                hg ^. lens
        mconcat
            [ jumpEventMap Config.holeJumpToNextKeys "next" NearestHoles.next
            , jumpEventMap Config.holeJumpToPrevKeys "previous" NearestHoles.prev
            ] & return
    where
        jumpDoc :: String -> String
        jumpDoc dirStr = "Jump to " ++ dirStr ++ " hole"

jumpHolesEventMapIfSelected ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (EventHandlers (T m))
jumpHolesEventMapIfSelected pl =
    do
        isSelected <- ExprGuiM.isExprSelected pl
        if isSelected
            then pl ^. Sugar.plData . ExprGuiT.plNearestHoles & jumpHolesEventMap
            else pure mempty

extractCursor :: Sugar.ExtractToDestination -> Widget.Id
extractCursor (Sugar.ExtractToLet letId) = WidgetIds.fromEntityId letId
extractCursor (Sugar.ExtractToDef defId) =
    WidgetIds.nameEditOf (WidgetIds.fromEntityId defId)

extractEventMap :: Functor m => Config -> Sugar.Actions m -> EventHandlers (T m)
extractEventMap config actions =
    actions ^. Sugar.extract <&> extractCursor
    & Widget.keysEventMapMovesCursor keys doc
    where
        doc = E.Doc ["Edit", "Extract"]
        keys = Config.extractKeys config

replaceOrComeToParentEventMap ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (EventHandlers (T m))
replaceOrComeToParentEventMap pl =
    do
        config <- ExprGuiM.readConfig
        isSelected <- ExprGuiM.isExprSelected pl
        return $
            if isSelected
            then replaceEventMap config (pl ^. Sugar.plActions)
            else
                Widget.keysEventMapMovesCursor (Config.delKeys config)
                (E.Doc ["Navigation", "Select parent"]) selectParent
    where
        selectParent =
            WidgetIds.fromExprPayload pl
            & WidgetIds.notDelegatingId
            & return

actionsEventMap ::
    Monad m =>
    HolePickers m -> IsHoleResult ->
    Sugar.Actions m ->
    ExprGuiM m (EventHandlers (T m))
actionsEventMap holePickers isHoleResult actions =
    do
        config <- ExprGuiM.readConfig
        return $ mconcat
            [ wrapEventMap holePickers config
            , applyOperatorEventMap holePickers
            , case isHoleResult of
              HoleResult -> mempty
              NotHoleResult -> extractEventMap config
            ] actions

applyOperatorEventMap ::
    Monad m => HolePickers m -> Sugar.Actions m -> EventHandlers (T m)
applyOperatorEventMap holePickers actions =
    case actions ^. Sugar.wrap of
    Sugar.WrapAction wrap -> action wrap
    Sugar.WrapperAlready holeId -> action $ return holeId
    Sugar.WrappedAlready holeId -> action $ return holeId
    Sugar.WrapNotAllowed -> mempty
    where
        doc = E.Doc ["Edit", ExprGuiM.holePickersAddDocPrefix holePickers "Apply operator"]
        action wrap =
            E.charGroup "Operator" doc operatorChars $ \c ->
                mappend
                <$> ExprGuiM.holePickersAction holePickers
                <*> do
                    (guid, entityId) <- wrap
                    cursor <- HoleEditState.setHoleStateAndJump guid (HoleState [c]) entityId
                    return $ Widget.eventResultFromCursor cursor

wrapEventMap ::
    Monad m =>
    HolePickers m -> Config ->
    Sugar.Actions m -> EventHandlers (T m)
wrapEventMap holePickers config actions =
    case actions ^. Sugar.wrap of
    Sugar.WrapAction wrap ->
        mkEventMapWithPickers holePickers
        (Config.wrapKeys config)
        (E.Doc ["Edit", ExprGuiM.holePickersAddDocPrefix holePickers "Wrap"])
        (fmap (HoleWidgetIds.hidOpen . HoleWidgetIds.make)) (snd <$> wrap)
    Sugar.WrapperAlready _ -> mempty
    Sugar.WrappedAlready _ -> mempty
    Sugar.WrapNotAllowed -> mempty

replaceEventMap :: Monad m => Config -> Sugar.Actions m -> EventHandlers (T m)
replaceEventMap config actions =
    mconcat
    [ case actions ^. Sugar.setToInnerExpr of
      Sugar.SetToInnerExpr action ->
          mk "Replace with inner expression" delKeys action
      Sugar.NoInnerExpr -> mempty
    , case actions ^. Sugar.setToHole of
      Sugar.SetToHole action ->
          mk "Delete expression" delKeys (fmap snd action)
      Sugar.SetWrapperToHole action ->
          mk "Delete outer hole" delKeys (fmap snd action)
      Sugar.AlreadyAHole -> mempty
      Sugar.AlreadyAppliedToHole -> mempty
    ]
    where
        mk doc keys = mkEventMap keys (E.Doc ["Edit", doc])
        delKeys = Config.delKeys config
        mkEventMap keys doc =
            Widget.keysEventMapMovesCursor keys doc .
            fmap WidgetIds.fromEntityId

modifyEventMap ::
    Monad m => HolePickers m -> Config ->
    Sugar.Actions m -> EventHandlers (T m)
modifyEventMap holePickers config =
    mconcat
    [ wrapEventMap holePickers config
    , applyOperatorEventMap holePickers
    , replaceEventMap config
    ]
