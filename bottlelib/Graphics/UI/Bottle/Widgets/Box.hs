{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.Box
    ( Box, KBox(..), Alignment
    , make, makeKeyed, makeAlign
    , unkey
    , boxMCursor, boxSize, boxContent, boxOrientation
    , Element, elementRect, elementAlign, elementOriginalWidget
    , Cursor, toWidget, toWidgetBiased
    , Orientation, horizontal, vertical
    , hboxAlign, vboxAlign
    , hboxCentered, vboxCentered
    , hbox, vbox
    ) where

import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Rect (Rect(..))
import           Graphics.UI.Bottle.Widget (Widget, Size)
import           Graphics.UI.Bottle.Widgets.Grid (KGrid(..))
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

type Cursor = Int

eHead :: [a] -> a
eHead (x:_) = x
eHead [] = error "Grid returned invalid list without any elements, instead of list Box handed it"

-- Want a 2d vector like in Grid, because we may want to preserve the
-- alignment in the resulting KBox.
type Alignment = Grid.Alignment

data Orientation = Orientation
    { oToGridCursor :: Cursor -> Grid.Cursor
    , oToGridChildren :: forall a. [a] -> [[a]]
    , oFromGridCursor :: Grid.Cursor -> Cursor
    , oFromGridChildren :: forall a. [[a]] -> [a]
    }

horizontal :: Orientation
horizontal = Orientation
    { oToGridCursor = (`Vector2` 0)
    , oToGridChildren = (: [])
    , oFromGridCursor = (^. _1)
    , oFromGridChildren = eHead
    }

vertical :: Orientation
vertical = Orientation
    { oToGridCursor = (0 `Vector2`)
    , oToGridChildren = map (: [])
    , oFromGridCursor = (^. _2)
    , oFromGridChildren = map eHead
    }

type Element = Grid.Element

{-# INLINE elementRect #-}
elementRect :: Lens.Getter (Element f) Rect
elementRect = Grid.elementRect

{-# INLINE elementAlign #-}
elementAlign :: Lens.Getter (Element f) Alignment
elementAlign = Grid.elementAlign

{-# INLINE elementOriginalWidget #-}
elementOriginalWidget :: Lens.Getter (Element f) (Widget f)
elementOriginalWidget = Grid.elementOriginalWidget

data KBox key f = KBox
    { __boxOrientation :: Orientation
    , __boxMCursor :: Maybe Cursor
    , __boxSize :: Size
    , __boxContent :: [(key, Element f)]
    , __boxGrid :: KGrid key f
    }
Lens.makeLenses ''KBox

{-# INLINE boxOrientation #-}
boxOrientation :: Lens.Getter (KBox key f) Orientation
boxOrientation = _boxOrientation

{-# INLINE boxMCursor #-}
boxMCursor :: Lens.Getter (KBox key f) (Maybe Cursor)
boxMCursor = _boxMCursor

{-# INLINE boxSize #-}
boxSize :: Lens.Getter (KBox key f) Size
boxSize = _boxSize

{-# INLINE boxContent #-}
boxContent :: Lens.Getter (KBox key f) [(key, Element f)]
boxContent = _boxContent

type Box = KBox ()

makeKeyed :: Orientation -> [(key, (Alignment, Widget f))] -> KBox key f
makeKeyed orientation children = KBox
    { __boxOrientation = orientation
    , __boxMCursor = fmap (oFromGridCursor orientation) $ grid ^. Grid.gridMCursor
    , __boxSize = grid ^. Grid.gridSize
    , __boxContent = oFromGridChildren orientation $ grid ^. Grid.gridContent
    , __boxGrid = grid
    }
    where
        grid = Grid.makeKeyed $ oToGridChildren orientation children

unkey :: [(Alignment, Widget f)] -> [((), (Alignment, Widget f))]
unkey = map ((,) ())

make :: Orientation -> [(Alignment, Widget f)] -> Box f
make orientation = makeKeyed orientation . unkey

makeAlign :: Alignment -> Orientation -> [Widget f] -> Box f
makeAlign alignment orientation = make orientation . map ((,) alignment)

toWidget :: KBox key f -> Widget f
toWidget = Grid.toWidget . __boxGrid

toWidgetBiased :: Cursor -> KBox key f -> Widget f
toWidgetBiased cursor box =
    Grid.toWidgetBiased gridCursor $ __boxGrid box
    where
        gridCursor = oToGridCursor (box ^. boxOrientation) cursor

boxAlign :: Orientation -> Alignment -> [Widget f] -> Widget f
boxAlign orientation align =
    toWidget .
    makeAlign align orientation

hboxAlign :: Alignment -> [Widget f] -> Widget f
hboxAlign = boxAlign horizontal

vboxAlign :: Alignment -> [Widget f] -> Widget f
vboxAlign = boxAlign vertical

vboxCentered :: [Widget f] -> Widget f
vboxCentered = vboxAlign 0.5

hboxCentered :: [Widget f] -> Widget f
hboxCentered = hboxAlign 0.5

hbox :: [(Alignment, Widget f)] -> Widget f
hbox = toWidget . make horizontal

vbox :: [(Alignment, Widget f)] -> Widget f
vbox = toWidget . make vertical
