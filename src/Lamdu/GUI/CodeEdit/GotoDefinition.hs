-- | A goto-definition widget
module Lamdu.GUI.CodeEdit.GotoDefinition
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString.Char8 as BS8
import           Data.MRUMemo (memo)
import qualified Data.Text as Text
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Fuzzy (Fuzzy)
import qualified Lamdu.Fuzzy as Fuzzy
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.I18N.Navigation (Navigation)
import qualified Lamdu.I18N.Navigation as Navigation
import           Lamdu.Name (Name)
import qualified Lamdu.Name as Name
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

myId :: Widget.Id
myId = Widget.Id ["goto-def"]

allowSearchTerm :: Text -> Bool
allowSearchTerm = Name.isValidText

{-# NOINLINE fuzzyMaker #-}
fuzzyMaker :: [(Text, Int)] -> Fuzzy (Set Int)
fuzzyMaker = memo Fuzzy.make

nameSearchTerm :: (MonadReader env m, Has (Texts.Name Text) env) => Name -> m Text
nameSearchTerm name =
    Name.visible name <&>
    \(Name.TagText text textCol, tagCol) ->
    text <> collisionText textCol <> collisionText tagCol
    where
        collisionText Name.NoCollision = ""
        collisionText (Name.Collision i) = Text.pack (show i)
        collisionText Name.UnknownCollision = "?"

makeOptions ::
    ( MonadReader env m, Has Theme env, Applicative o
    , Has TextView.Style env, Element.HasAnimIdPrefix env, GuiState.HasCursor env
    , Has (Navigation Text) env, Has (Texts.Name Text) env, Has Dir.Layout env
    ) =>
    m [Sugar.NameRef Name o] ->
    SearchMenu.ResultsContext -> m (Menu.OptionList (Menu.Option m o))
makeOptions readGlobals (SearchMenu.ResultsContext searchTerm prefix)
    | Text.null searchTerm = pure Menu.TooMany
    | otherwise =
        do
            goto <- Lens.view (has . Navigation.goto)
            let toRenderedOption nameRef widget =
                    Menu.RenderedOption
                    { Menu._rWidget = widget
                    , Menu._rPick =
                        Widget.PreEvent
                        { Widget._pDesc = goto
                        , Widget._pAction =
                            nameRef ^. Sugar.nrGotoDefinition
                            <&> WidgetIds.fromEntityId <&> toPickResult
                        , Widget._pTextRemainder = ""
                        }
                    }
            let makeOption (idx, nameRef) =
                    GetVarEdit.makeSimpleView TextColors.definitionColor name optId
                    <&> toRenderedOption nameRef
                    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId optId)
                    & wrapOption optId
                    where
                        name = nameRef ^. Sugar.nrName
                        optId = prefix `Widget.joinId` [BS8.pack (show idx)]
            readGlobals <&> zip [0::Int ..]
                >>= traverse withText
                <&> (Fuzzy.memoableMake fuzzyMaker ?? searchTerm)
                <&> map (makeOption . snd)
                <&> Menu.FullList
    where
        withText (idx, nameRef) =
            nameSearchTerm (nameRef ^. Sugar.nrName) <&> \x -> (x, (idx, nameRef))
        wrapOption optId mkRenedered =
            Menu.Option
            { Menu._oId = optId
            , Menu._oRender = mkRenedered
            , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
            }
        toPickResult x = Menu.PickResult x (Just x)

make ::
    ( MonadReader env m, Applicative o
    , Has Theme env, Element.HasAnimIdPrefix env
    , Has Menu.Config env, Has Hover.Style env, GuiState.HasState env
    , Has SearchMenu.TermStyle env, Has (Navigation Text) env
    , Glue.HasTexts env, TextEdit.Deps env
    , SearchMenu.HasTexts env, Has (Texts.Name Text) env
    ) =>
    m [Sugar.NameRef Name o] -> m (StatusBar.StatusWidget o)
make readGlobals =
    do
        goto <- Lens.view (has . Navigation.goto)
        SearchMenu.make (SearchMenu.searchTermEdit myId (pure . allowSearchTerm))
            (makeOptions readGlobals) Element.empty myId ?? Menu.Below
            & Reader.local (has . Theme.searchTerm %~ onTermStyle goto)
            <&> \searchWidget -> StatusBar.StatusWidget
            { StatusBar._widget = searchWidget
            , StatusBar._globalEventMap = mempty
            }
    where
        onTermStyle goto x =
            x
            & SearchMenu.emptyStrings . Lens.mapped .~ goto
            & SearchMenu.bgColors . Lens.mapped .~ Draw.Color 0 0 0 0
