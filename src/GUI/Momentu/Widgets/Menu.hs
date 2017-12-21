{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings, DeriveTraversable, FlexibleContexts, DisambiguateRecordFields #-}

module GUI.Momentu.Widgets.Menu
    ( Style(..), HasStyle(..)
    , Submenu(..), _SubmenuEmpty, _SubmenuItems
    , Option(..), oId, oWidget, oSubmenuWidgets
    , optionWidgets
    , Placement(..), HasMoreOptions(..)
    , make, makeHoverBeside
    , makeHovered
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import           GUI.Momentu.Align (WithTextPos, Aligned(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/), (/-/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Lamdu.Prelude

data Style = Style
    { submenuSymbolColorUnselected :: Draw.Color
    , submenuSymbolColorSelected :: Draw.Color
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Style

class HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

data Submenu f a
    = SubmenuEmpty
    | SubmenuItems (f [WithTextPos (Widget a)])
    deriving (Functor)
Lens.makePrisms ''Submenu

data Option f a = Option
    { -- | Must be the prefix of all submenu options, also used to
      -- create this option's submenu arrow frame:
      _oId :: !Widget.Id
    , -- A widget that represents this option
      _oWidget :: !(WithTextPos (Widget a))
    , -- An optionally empty submenu
      _oSubmenuWidgets :: !(Submenu f a)
    }
Lens.makeLenses ''Option

optionWidgets :: Functor f => Lens.Setter (Option f a) (Option f b) (WithTextPos (Widget a)) (WithTextPos (Widget b))
optionWidgets f (Option i w s) =
    Option i <$> f w <*> (_SubmenuItems . Lens.mapped . Lens.mapped) f s

data HasMoreOptions = MoreOptionsAvailable | NoMoreOptions

makeNoResults ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    m (WithTextPos View)
makeNoResults = TextView.makeLabel "(No results)"

makeMoreOptionsView ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    HasMoreOptions -> m (WithTextPos View)
makeMoreOptionsView NoMoreOptions = pure Element.empty
makeMoreOptionsView MoreOptionsAvailable = TextView.makeLabel "..."

blockEvents ::
    Applicative f =>
    Hover.Ordered (Widget (f State.Update) -> Widget (f State.Update))
blockEvents =
    Hover.Ordered
    { _forward = blockDirection MetaKey.Key'Down "down"
    , _backward = blockDirection MetaKey.Key'Up "up"
    }
    where
        blockDirection key keyName =
            pure mempty
            & E.keyPresses
                [ModKey mempty key]
                (E.Doc ["Navigation", "Move", keyName <> " (blocked)"])
            & E.weakerEvents


submenuSymbolText :: Text
submenuSymbolText = " ▷"

makeSubmenuSymbol ::
    ( MonadReader env m, HasStyle env, Element.HasAnimIdPrefix env
    , TextView.HasStyle env
    ) =>
    Bool -> m (WithTextPos View)
makeSubmenuSymbol isSelected =
    do
        color <- Lens.view style <&> submenuSymbolColor
        TextView.makeLabel submenuSymbolText
            & Reader.local (TextView.color .~ color)
    where
        submenuSymbolColor
            | isSelected = submenuSymbolColorSelected
            | otherwise = submenuSymbolColorUnselected

layoutOption ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    , State.HasCursor env, Hover.HasStyle env, HasStyle env, Functor f
    ) =>
    Widget.R -> Option m (f State.Update) -> m (WithTextPos (Widget (f State.Update)))
layoutOption maxOptionWidth option =
    case option ^. oSubmenuWidgets of
    SubmenuEmpty -> singular
    SubmenuItems action ->
        do
            isOnSubmenu <- State.isSubCursor ?? option ^. oId
            let isSelected = isOnSubmenu || isOnOption
            submenuSymbol <-
                makeSubmenuSymbol isSelected
            let base =
                    (option ^. oWidget
                     & Element.width .~ maxOptionWidth - submenuSymbol ^. Element.width)
                    /|/ submenuSymbol
            if isSelected
                then do
                    hover <- Hover.hover
                    submenus <- action
                    let anchored = base & Align.tValue %~ Hover.anchor
                    anchored
                        & Align.tValue %~
                        Hover.hoverInPlaceOf
                        (Hover.hoverBesideOptionsAxis Glue.Horizontal
                         Hover.Ordered
                         { _forward = Glue.vbox submenus <&> hover
                         , _backward = Glue.vbox submenus <&> hover
                         } anchored <&> (^. Align.tValue))
                        & pure
                else pure base
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        isOnOption = Widget.isFocused (option ^. oWidget . Align.tValue)
        singular = option ^. oWidget & Element.width .~ maxOptionWidth & pure
        animId = option ^. oId & Widget.toAnimId

make ::
    ( MonadReader env m, TextView.HasStyle env, Hover.HasStyle env
    , Element.HasAnimIdPrefix env, HasStyle env, State.HasCursor env
    , Applicative f
    ) =>
    Widget.R -> [Option m (f State.Update)] -> HasMoreOptions ->
    m (Hover.Ordered (Widget (f State.Update)))
make minWidth options hiddenResults =
    case options of
    [] -> makeNoResults <&> (^. Align.tValue) <&> Widget.fromView <&> pure
    _:_ ->
        do
            submenuSymbolWidth <-
                TextView.drawText ?? submenuSymbolText
                <&> (^. TextView.renderedTextSize . TextView.bounding . _1)
            let optionMinWidth option =
                    option ^. oWidget . Element.width +
                    case option ^. oSubmenuWidgets of
                    SubmenuEmpty -> 0
                    SubmenuItems {} -> submenuSymbolWidth
            let maxOptionWidth = options <&> optionMinWidth & maximum & max minWidth
            hiddenOptionsWidget <-
                makeMoreOptionsView hiddenResults
                <&> (^. Align.tValue) <&> Widget.fromView
            laidOutOptions <-
                traverse (layoutOption maxOptionWidth) options
                <&> map (^. Align.tValue)
            blockEvents <*>
                ( Hover.Ordered
                    { _forward = id
                    , _backward = reverse
                    } ?? (laidOutOptions ++ [hiddenOptionsWidget])
                    <&> Glue.vbox
                ) & pure

makeHoverBeside ::
    ( MonadReader env m, TextView.HasStyle env, Hover.HasStyle env
    , Element.HasAnimIdPrefix env, HasStyle env, State.HasCursor env
    , Applicative f
    ) =>
    Widget.R -> [Option m (f State.Update)] -> HasMoreOptions ->
    Widget (f State.Update) ->
    m (Widget (f State.Update))
makeHoverBeside minWidth options hiddenResults baseWidget =
    (Hover.hoverBesideAxis ?? Hover.Vertical ?? baseWidget)
    <*> make minWidth options hiddenResults

-- | You may want to limit the placement of hovering pop-up menus,
-- so that they don't cover other ui elements.
data Placement = Above | Below | AnyPlace

hoverOptions ::
    ( MonadReader env m, Hover.HasStyle env, Element.HasAnimIdPrefix env
    , Functor f
    ) =>
    m ( Placement ->
        View ->
        Hover.Ordered (Widget (f State.Update)) ->
        Hover.AnchoredWidget (f State.Update) ->
        [Hover.AnchoredWidget (f State.Update)]
      )
hoverOptions =
    Hover.hover <&>
    \hover pos annotation results searchTerm ->
    let resultsAbove alignment =
            results ^. Hover.backward & hover & Aligned alignment
        annotatedTerm alignment = searchTerm & Widget.widget %~ (/-/ annotation) & Aligned alignment
        aboveRight = resultsAbove 0 /-/ annotatedTerm 0
        aboveLeft =
            resultsAbove 1
            /-/ annotatedTerm 1
        annotatedResultsBelow = (results ^. Hover.forward) /-/ annotation & hover
        resultsBelow = results ^. Hover.forward & hover
        belowRight =
            Aligned 0 searchTerm
            /-/
            Aligned 0 annotatedResultsBelow
        belowLeft =
            Aligned 1 searchTerm
            /-/
            Aligned 1 annotatedResultsBelow
        centerRight = annotatedTerm 0.5 /|/ Aligned 0.5 resultsBelow
        rightAbove = annotatedTerm 1 /|/ resultsAbove 1
        leftAbove = resultsAbove 1 /|/ annotatedTerm 1
    in
    case pos of
    Above ->
        [ aboveRight
        , aboveLeft
        ]
    AnyPlace ->
        [ belowRight
        , aboveRight
        , belowLeft
        , aboveLeft
        , centerRight
        ]
    Below ->
        [ belowRight
        , belowLeft
        , rightAbove
        , leftAbove
        ]
    <&> (^. Align.value)

makeHovered ::
    ( Applicative f, State.HasCursor env, HasStyle env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env
    , Hover.HasStyle env, MonadReader env m
    ) =>
    View -> [Option m (f State.Update)] -> HasMoreOptions ->
    m (Placement -> Widget (f State.Update) -> Widget (f State.Update))
makeHovered annotation options hasHiddenResults =
    do
        mkHoverOptions <- hoverOptions
        menu <- make (annotation ^. Element.width) options hasHiddenResults
        pure $
            \placement term ->
            let a = Hover.anchor term
            in  a
                & Hover.hoverInPlaceOf (mkHoverOptions placement annotation menu a)
