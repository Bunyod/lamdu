{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.Grid
  ( Grid, KGrid(..)
  , make, makeKeyed, makeAlign, makeCentered
  , unkey
  , Alignment
  , atGridMCursor
  , atGridContent
  , Element(..)
  , atElementRect
  , atElementW
  , Cursor, toWidget, toWidgetBiased
  ) where

import Control.Applicative (liftA2)
import Control.Arrow (first, second)
import Control.Lens ((^.))
import Control.Monad (msum, (>=>))
import Data.Function (on)
import Data.List (foldl', transpose, find, minimumBy, sortBy, groupBy)
import Data.List.Utils (index, enumerate2d)
import Data.MRUMemo (memo)
import Data.Maybe (isJust, fromMaybe, catMaybes, mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Ord (comparing)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.Widget (Widget(..), R)
import Graphics.UI.Bottle.Widgets.GridView (Alignment)
import Graphics.UI.Bottle.Widgets.StdKeys (DirKeys(..), stdDirKeys)
import qualified Control.Lens as Lens
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.GLFW as GLFW

type Cursor = Vector2 Int

length2d :: [[a]] -> Vector2 Int
length2d xs = Vector2 (foldl' max 0 . map length $ xs) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (fmap (subtract 1) size)

data NavDests f = NavDests
  { leftOfCursor
  , aboveCursor
  , rightOfCursor
  , belowCursor
  , topCursor
  , leftMostCursor
  , bottomCursor
  , rightMostCursor :: Maybe (Widget.EnterResult f)
  }

mkNavDests :: Widget.Size -> Rect -> [[Widget.MEnter f]] -> Cursor -> NavDests f
mkNavDests widgetSize prevFocalArea mEnterss cursor@(Vector2 cursorX cursorY) = NavDests
  { leftOfCursor    = givePrevFocalArea . reverse $ take cursorX curRow
  , aboveCursor     = givePrevFocalArea . reverse $ take cursorY curColumn
  , rightOfCursor   = givePrevFocalArea $ drop (cursorX+1) curRow
  , belowCursor     = givePrevFocalArea $ drop (cursorY+1) curColumn

  , topCursor       = giveEdge (Vector2 Nothing (Just 0)) $ take (min 1 cursorY) curColumn
  , leftMostCursor  = giveEdge (Vector2 (Just 0) Nothing) $ take (min 1 cursorX) curRow
  , bottomCursor    = giveEdge (Vector2 Nothing (Just 1)) . take 1 . reverse $ drop (cursorY+1) curColumn
  , rightMostCursor = giveEdge (Vector2 (Just 1) Nothing) . take 1 . reverse $ drop (cursorX+1) curRow
  }
  where
    curRow = fromMaybe [] $ index cappedY mEnterss
    curColumn = fromMaybe [] $ index cappedX (transpose mEnterss)
    Vector2 cappedX cappedY = capCursor size cursor
    size = length2d mEnterss

    give rect = fmap ($ Direction.PrevFocalArea rect) . msum
    givePrevFocalArea = give prevFocalArea
    giveEdge edge = give Rect
      { Rect._topLeft =
          liftA2 fromMaybe (Rect._topLeft prevFocalArea) $
          liftA2 (fmap . (*)) widgetSize edge
      , Rect._size =
          liftA2 fromMaybe (Rect._size prevFocalArea) $
          (fmap . fmap) (const 0) edge
      }


mkNavEventmap
  :: NavDests f -> (Widget.EventHandlers f, Widget.EventHandlers f)
mkNavEventmap navDests = (weakMap, strongMap)
  where
    weakMap = mconcat . catMaybes $ [
      movementk "left"       (keysLeft  stdDirKeys) leftOfCursor,
      movementk "right"      (keysRight stdDirKeys) rightOfCursor,
      movementk "up"         (keysUp    stdDirKeys) aboveCursor,
      movementk "down"       (keysDown  stdDirKeys) belowCursor,
      movementk "more left"  [GLFW.KeyHome]         leftMostCursor,
      movementk "more right" [GLFW.KeyEnd]          rightMostCursor
      ]
    strongMap = mconcat . catMaybes $ [
      movementk "top"       [GLFW.KeyPageup]    topCursor,
      movementk "bottom"    [GLFW.KeyPagedown]  bottomCursor,
      movement "leftmost"  [ctrlK GLFW.KeyHome] leftMostCursor,
      movement "rightmost" [ctrlK GLFW.KeyEnd]  rightMostCursor
      ]
    k = EventMap.ModKey EventMap.noMods
    ctrlK = EventMap.ModKey EventMap.ctrl
    movementk dirName = movement dirName . map k
    movement dirName events =
      fmap
        (EventMap.keyPresses
         events
         ("Move " ++ dirName) .
         Widget.enterResultEvent) .
      ($ navDests)

getCursor :: [[Widget k]] -> Maybe Cursor
getCursor =
  fmap cursorOf . find (wIsFocused . snd) . concat . enumerate2d
  where
    cursorOf ((row, column), _) = Vector2 column row

data Element f = Element
  { elementAlign :: Alignment
  , elementRect :: Rect
  , elementW :: Widget f
  }

data KGrid key f = KGrid
  { gridMCursor :: Maybe Cursor
  , gridSize :: Widget.Size
  , gridContent :: [[(key, Element f)]]
  }

AtFieldTH.make ''Element
AtFieldTH.make ''KGrid

type Grid = KGrid ()

makeKeyed :: [[(key, (Alignment, Widget f))]] -> KGrid key f
makeKeyed children = KGrid
  { gridMCursor = getCursor $ (map . map) (snd . snd) children
  , gridSize = size
  , gridContent = content
  }
  where
    (size, content) =
      GridView.makeGeneric translate $
      (map . map) mkSizedKeyedContent children
    mkSizedKeyedContent (key, (alignment, widget)) =
      ((Widget.wSize widget, alignment), (key, widget))
    translate align rect =
      second
      (Element align rect .
       Widget.translate (rect ^. Rect.topLeft))

unkey :: [[(Alignment, Widget f)]] -> [[((), (Alignment, Widget f))]]
unkey = (map . map) ((,) ())

make :: [[(Alignment, Widget f)]] -> Grid f
make = makeKeyed . unkey

makeAlign :: Alignment -> [[Widget f]] -> Grid f
makeAlign alignment = make . (map . map) ((,) alignment)

makeCentered :: [[Widget f]] -> Grid f
makeCentered = makeAlign 0.5


helper ::
  (Widget.Size -> [[Widget.MEnter f]] -> Widget.MEnter f) ->
  KGrid key f -> Widget f
helper combineEnters (KGrid mCursor size sChildren) =
  combineWs $ (map . map) (elementW . snd) sChildren
  where
    combineWs wss =
      maybe unselectedW makeW mCursor
      where
        framess = (map . map) wFrame wss
        mEnterss = (map . map) wMaybeEnter wss
        frame = mconcat $ concat framess
        mEnter = combineEnters size mEnterss

        unselectedW = Widget
          { wIsFocused = isJust mCursor
          , wSize = size
          , wFrame = frame
          , wMaybeEnter = mEnter
          , wEventMap = mempty
          , wFocalArea = Rect 0 size
          }

        makeW cursor@(Vector2 x y) = Widget
          { wIsFocused = isJust mCursor
          , wSize = size
          , wFrame = frame
          , wMaybeEnter = mEnter
          , wEventMap = makeEventMap w navDests
          , wFocalArea = wFocalArea w
          }
          where
            navDests = mkNavDests size (wFocalArea w) mEnterss cursor
            w = wss !! y !! x

        makeEventMap w navDests =
          mconcat [strongMap, wEventMap w, weakMap]
          where
            (weakMap, strongMap) = mkNavEventmap navDests

tupleToVector2 :: (a, a) -> Vector2 a
tupleToVector2 (y, x) = Vector2 x y

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . comparing

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . ((==) `on`)

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = groupOn f . sortOn f

toWidget :: KGrid key f -> Widget f
toWidget =
  helper makeMEnter
  where
    makeMEnter size children = chooseClosest childEnters
      where
        childEnters =
          mapMaybe indexIntoMaybe .
          (concatMap . map . first) tupleToVector2 $
          enumerate2d children

        chooseClosest [] = Nothing
        chooseClosest _ = Just byDirection

        byDirection dir =
          minimumOn
          (Vector2.uncurry (+) . abs . modifyDistance .
           distance dirRect . Widget.enterResultRect) .
          map ($ dir) $ filteredByEdge edge
          where
            removeUninterestingAxis = ((1 - abs (fmap fromIntegral edge)) *)
            (modifyDistance, dirRect) = case dir of
              Direction.Outside -> (id, Rect 0 0)
              Direction.PrevFocalArea x -> (removeUninterestingAxis, x)
              Direction.Point x -> (id, Rect x 0)
            edge = asEdge size dirRect

        distance = (-) `on` Lens.view Rect.center

        filteredByEdge = memo $ \(Vector2 hEdge vEdge) ->
          map snd .
          safeHead . groupSortOn ((* (-hEdge)) . Lens.view Vector2.first . fst) .
          safeHead . groupSortOn ((* (-vEdge)) . Lens.view Vector2.second . fst) $
          childEnters
        indexIntoMaybe (i, m) = fmap ((,) i) m

safeHead :: Monoid a => [a] -> a
safeHead = mconcat . take 1

asEdge :: Vector2 R -> Rect -> Vector2 Int
asEdge size rect =
  Vector2 hEdge vEdge
  where
    hEdge = boolToInt rightEdge - boolToInt leftEdge
    vEdge = boolToInt bottomEdge - boolToInt topEdge
    boolToInt False = 0
    boolToInt True = 1
    Vector2 leftEdge topEdge =
      fmap (<= 0) (rect ^. Rect.bottomRight)
    Vector2 rightEdge bottomEdge =
      liftA2 (>=) (rect ^. Rect.topLeft) size

-- ^ If unfocused, will enters the given child when entered
toWidgetBiased :: Cursor -> KGrid key f -> Widget f
toWidgetBiased (Vector2 x y) = helper . const $ index y >=> index x >=> id
