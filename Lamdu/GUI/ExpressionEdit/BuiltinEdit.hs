{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.BuiltinEdit
    ( make
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import qualified Data.List as List
import           Data.List.Split (splitOn)
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Definition as Definition
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

builtinFDConfig :: FocusDelegator.Config
builtinFDConfig = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [ModKey mempty GLFW.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Change imported name"]
    , FocusDelegator.focusParentKeys = [ModKey mempty GLFW.Key'Escape]
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Stop changing name"]
    }

builtinFFIPath :: Widget.Id -> Widget.Id
builtinFFIPath = flip Widget.joinId ["FFIPath"]

builtinFFIName :: Widget.Id -> Widget.Id
builtinFFIName = flip Widget.joinId ["FFIName"]

makeNamePartEditor ::
    (Monad f, Monad m) =>
    Draw.Color -> String -> (String -> f ()) -> Widget.Id ->
    ExprGuiM m (Widget (f Widget.EventResult))
makeNamePartEditor color namePartStr setter myId =
    ExprGuiM.makeFocusDelegator builtinFDConfig FocusDelegator.FocusEntryParent myId
    <*> ExprGuiM.widgetEnv
        ( BWidgets.makeWordEdit (Property namePartStr setter)
          (myId `Widget.joinId` ["textedit"])
        )
    & ExprGuiM.withFgColor color

make ::
    Monad m =>
    Sugar.DefinitionBuiltin m -> Widget.Id ->
    ExprGuiM m (Widget (T m Widget.EventResult))
make def myId =
    do
        config <- ExprGuiM.readConfig
        moduleName <-
            makeNamePartEditor (Config.foreignModuleColor config)
            modulePathStr modulePathSetter (builtinFFIPath myId)
        varName <-
            makeNamePartEditor (Config.foreignVarColor config) name nameSetter
            (builtinFFIName myId)
        dot <- ExprGuiM.makeLabel "." $ Widget.toAnimId myId
        [moduleName, dot, varName]
            & Box.hboxCentered
            & return
    & ExprGuiM.assignCursor myId (builtinFFIName myId)
    where
        Sugar.DefinitionBuiltin
            (Definition.FFIName modulePath name) setFFIName _ = def
        modulePathStr = List.intercalate "." modulePath
        modulePathSetter = setFFIName . (`Definition.FFIName` name) . splitOn "."
        nameSetter = setFFIName . Definition.FFIName modulePath
