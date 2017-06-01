{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.UI.Bottle.Direction
    ( Direction(..), coordinates
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Rect (R, Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect

import           Lamdu.Prelude

-- RelativePos pos is relative to the top-left of the widget
data Direction = Outside | PrevFocalArea Rect | Point (Vector2 R)

coordinates :: Lens.Traversal' Direction Rect
coordinates _ Outside = pure Outside
coordinates f (PrevFocalArea x) = PrevFocalArea <$> f x
coordinates f (Point x) =
    Point . (^. Rect.topLeft) <$> f (Rect x 0)
