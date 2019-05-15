-- | A vertical-expand (combo-like) choice widget
{-# LANGUAGE TemplateHaskell, DerivingVia, FlexibleContexts #-}
module GUI.Momentu.Widgets.Choice
    ( make
    , defaultFdConfig
    , Config(..), defaultConfig
    , Orientation(..)
    , Texts(..), select, chooseSelected
    , HasTexts(..)
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Has (Has)
import           Data.Property (Property(..))
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Direction (Orientation(..), perpendicular, axis)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator

import           Lamdu.Prelude

data Texts a = Texts
    { _select :: a
    , _chooseSelected :: a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Texts)

Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts
class Glue.HasTexts env => HasTexts env where texts :: Lens' env (Texts Text)


defaultFdConfig ::
    (MonadReader env m, HasTexts env) => m (E.Subtitle -> FocusDelegator.Config)
defaultFdConfig =
    Lens.view texts <&> \txt helpCategory ->
    FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [MetaKey noMods MetaKey.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc [helpCategory, txt ^. select]
    , FocusDelegator.focusParentKeys = [MetaKey.Key'Enter, MetaKey.Key'Escape] <&> MetaKey noMods
    , FocusDelegator.focusParentDoc = E.Doc [helpCategory, txt ^. chooseSelected]
    }

data Config = Config
    { cwcFDConfig :: FocusDelegator.Config
    , cwcOrientation :: Orientation
    }

defaultConfig :: (MonadReader env m, HasTexts env) => m (E.Subtitle -> Config)
defaultConfig =
    defaultFdConfig <&> \defFd helpCategory ->
    Config
    { cwcFDConfig = defFd helpCategory
    , cwcOrientation = Vertical
    }

data IsSelected = Selected | NotSelected
    deriving Eq

make ::
    ( Eq childId, MonadReader env m, Applicative f
    , State.HasCursor env, Has Hover.Style env, Element.HasAnimIdPrefix env
    , Glue.HasTexts env
    ) =>
    m
    (Property f childId -> [(childId, TextWidget f)] ->
     Config -> Widget.Id -> TextWidget f)
make =
    (,,,,)
    <$> Element.padToSize
    <*> Glue.box
    <*> Hover.hover
    <*> Hover.anchor
    <*> FocusDelegator.make
    <&> \(padToSize, box, hover, anc, fd)
         (Property curChild choose) children config myId ->
    let orientation = cwcOrientation config
        perp :: Lens' (Vector2 a) a
        perp = axis (perpendicular orientation)
        maxDim = children <&> (^. _2 . Element.size . perp) & maximum
        hoverAsClosed open =
            [hover (anc open)]
            `Hover.hoverInPlaceOf` anc (widget False ^. Align.tValue)
        widget allowExpand =
            children <&> annotate
            <&> prependEntryAction
            & filterVisible allowExpand
            <&> snd
            & box orientation
            <&> fd (cwcFDConfig config) FocusDelegator.FocusEntryParent myId
        filterVisible allowExpand
            | allowExpand && anyChildFocused = id
            | otherwise = filter ((== Selected) . fst)
        prependEntryAction (isSelected, action, w) =
            ( isSelected
            , w
                & Align.tValue . Widget.wState . Widget._StateUnfocused .
                Widget.uMEnter . Lens._Just . Lens.mapped .
                Widget.enterResultEvent
                    %~ (action *>)
            )
        anyChildFocused =
            Lens.orOf (Lens.traversed . _2 . Align.tValue . Lens.to Widget.isFocused) children
        annotate (item, w) =
            ( if item == curChild then Selected else NotSelected
            , choose item
            , w
            )
    in  widget True
        <&> (if anyChildFocused then hoverAsClosed else id)
        & padToSize (0 & perp .~ maxDim) 0
