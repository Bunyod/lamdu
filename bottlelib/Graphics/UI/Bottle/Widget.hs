{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Graphics.UI.Bottle.Widget
  ( module Graphics.UI.Bottle.WidgetId
  , Widget(..), MEnter, R, Size
  , EnterResult(..), enterResultEvent, enterResultRect
  , EventHandlers
  , EventResult(..), eventResultFromCursor
  , keysEventMap, keysEventMapMovesCursor
  , eAnimIdMapping, eCursor
  , wMaybeEnter, wEventMap, wFrame, wFocalArea
  , wIsFocused, wSize
  , atWFrameWithSize, atEvents
  , takesFocus, doesntTakeFocus
  , backgroundColor, tint, liftView
  , addInnerFrame
  , strongerEvents, weakerEvents
  , translate, translateBy, scale, scaleDownContent, pad, assymetricPad
  , overlayView
  ) where

import           Control.Applicative ((<$>), liftA2)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Monoid (Monoid(..))
import qualified Data.Monoid as Monoid
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           Data.Vector.Vector2 (Vector2(..))
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, R, Size)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Direction (Direction)
import qualified Graphics.UI.Bottle.Direction as Direction
import           Graphics.UI.Bottle.EventMap (EventMap)
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.ModKey (ModKey)
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View)
import           Graphics.UI.Bottle.WidgetId (Id(..), augmentId, toAnimId, joinId, subId)

data EventResult = EventResult
  { _eCursor :: Monoid.Last Id
  , _eAnimIdMapping :: Monoid.Endo AnimId
  } deriving (Generic)
instance Monoid EventResult where
  mempty = def_mempty
  mappend = def_mappend

data EnterResult f = EnterResult
  { _enterResultRect :: Rect
  , _enterResultEvent :: f EventResult
  }

type MEnter f = Maybe (Direction -> EnterResult f)
type EventHandlers f = EventMap (f EventResult)

data Widget f = Widget
  { _wIsFocused :: Bool
  , _wSize :: Size
  , _wFrame :: Anim.Frame
  , _wMaybeEnter :: MEnter f -- Nothing if we're not enterable
  , _wEventMap :: EventHandlers f
  , _wFocalArea :: Rect
  }

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventResult
Lens.makeLenses ''Widget

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor cursor = EventResult
  { _eCursor = Monoid.Last $ Just cursor
  , _eAnimIdMapping = mempty
  }

atEvents :: (f EventResult -> g EventResult) -> Widget f -> Widget g
atEvents func w = w
  { _wMaybeEnter =
       (Lens.mapped . Lens.mapped . enterResultEvent %~ func) $
       _wMaybeEnter w
  , _wEventMap = func <$> _wEventMap w
  }

liftView :: View -> Widget f
liftView (sz, frame) =
  Widget
    { _wIsFocused = False
    , _wSize = sz
    , _wFocalArea = Rect 0 sz
    , _wFrame = frame
    , _wEventMap = mempty
    , _wMaybeEnter = Nothing
    }

atWFrameWithSize :: (Size -> Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atWFrameWithSize f w = w & wFrame %~ f (w ^. wSize)

-- TODO: Would be nicer as (Direction -> Id), but then TextEdit's "f" couldn't be ((,) String)..
takesFocus :: Functor f => (Direction -> f Id) -> Widget f -> Widget f
takesFocus enter w = w & wMaybeEnter .~ mEnter
  where
    mEnter =
      Just $
      EnterResult focalArea .
      fmap eventResultFromCursor <$> enter
    focalArea = w ^. wFocalArea

doesntTakeFocus :: Widget f -> Widget f
doesntTakeFocus = wMaybeEnter .~ Nothing

-- ^ If doesn't take focus, event map is ignored
strongerEvents :: EventHandlers f -> Widget f -> Widget f
strongerEvents events = wEventMap %~ (events `mappend`)

-- ^ If doesn't take focus, event map is ignored
weakerEvents :: EventHandlers f -> Widget f -> Widget f
weakerEvents events = wEventMap %~ (`mappend` events)

backgroundColor :: Int -> AnimId -> Draw.Color -> Widget f -> Widget f
backgroundColor layer animId = atWFrameWithSize . Anim.backgroundColor animId layer

addInnerFrame :: Int -> AnimId -> Draw.Color -> Vector2 R -> Widget f -> Widget f
addInnerFrame layer animId color frameWidth =
  atWFrameWithSize f
  where
    f size =
      mappend $
      Anim.onDepth (+ layer) $ Anim.onImages (Draw.tint color) $
      Anim.emptyRectangle frameWidth size animId

tint :: Draw.Color -> Widget f -> Widget f
tint color = wFrame %~ Anim.onImages (Draw.tint color)

keysEventMap ::
  Functor f => [ModKey] -> EventMap.Doc ->
  f () -> EventHandlers f
keysEventMap keys doc act =
  (fmap . const) mempty <$>
  EventMap.keyPresses keys doc act

keysEventMapMovesCursor ::
  Functor f => [ModKey] -> EventMap.Doc ->
  f Id -> EventHandlers f
keysEventMapMovesCursor keys doc act =
  fmap eventResultFromCursor <$>
  EventMap.keyPresses keys doc act

-- TODO: This actually makes an incorrect widget because its size
-- remains same, but it is now translated away from 0..size
translate :: Vector2 R -> Widget f -> Widget f
translate pos =
  (wFrame %~ Anim.translate pos) .
  (wFocalArea . Rect.topLeft %~ (+pos)) .
  (wMaybeEnter . Lens.mapped %~
    (Lens.mapped . enterResultRect .
     Rect.topLeft %~ (+ pos)) .
    (Lens.argument . Direction.coordinates .
     Rect.topLeft %~ subtract pos))

translateBy :: (Vector2 R -> Vector2 R) -> Widget f -> Widget f
translateBy mkPos w =
  (translate . mkPos . (^. wSize)) w w

scale :: Vector2 R -> Widget f -> Widget f
scale mult =
  (wFrame %~ Anim.scale mult) .
  (wFocalArea . Rect.topLeftAndSize %~ (* mult)) .
  (wMaybeEnter . Lens.traversed %~
    (Lens.mapped . enterResultRect .
     Rect.topLeftAndSize %~ (*mult)) .
    (Lens.argument . Direction.coordinates .
     Rect.topLeftAndSize %~ (/mult))) .
  (wSize %~ (* mult))

-- | Scale down a widget without affecting its exported size
scaleDownContent :: Vector2 R -> Vector2 R -> Widget f -> Widget f
scaleDownContent factor align w =
  w
  & scale factor
  & translate ((w ^. wSize) * align * (1 - factor))
  & wSize .~ (w ^. wSize)

-- Surround a widget with padding
pad :: Vector2 R -> Widget f -> Widget f
pad p = assymetricPad p p

assymetricPad :: Vector2 R -> Vector2 R -> Widget f -> Widget f
assymetricPad leftAndTop rightAndBottom w =
  w
  & wSize %~ (+ (leftAndTop + rightAndBottom))
  & translate leftAndTop


overlayView :: View -> Widget f -> Widget f
overlayView (size, frame) w =
  w
  & wSize %~ liftA2 max size
  & wFrame %~ mappend frame
