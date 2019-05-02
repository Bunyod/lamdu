{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Direction
    ( Orientation(..), _Horizontal, _Vertical
    , perpendicular, axis, rectRange
    , Order(..), _Forward, _Backward
    , reverseOrder, applyOrder
    , englishName
    , Layout(..), _LeftToRight, _RightToLeft
    , HasLayoutDir(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.String (IsString(..))
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Rect (Rect, R)
import qualified GUI.Momentu.Rect as Rect

import           Lamdu.Prelude

data Layout
    = LeftToRight -- ^ e.g: latin languages
    | RightToLeft -- ^ e.g: Hebrew/Arabic
    deriving (Eq, Ord, Show)
deriveJSON Aeson.defaultOptions ''Layout
Lens.makePrisms ''Layout

class HasLayoutDir env where layoutDir :: Lens' env Layout
instance HasLayoutDir Layout where layoutDir = id

data Orientation = Horizontal | Vertical
    deriving (Eq, Show, Ord, Generic)

axis :: Functor f => Orientation -> Lens.LensLike' f (Vector2 a) a
axis Horizontal = _1
axis Vertical = _2

perpendicular :: Orientation -> Orientation
perpendicular Horizontal = Vertical
perpendicular Vertical = Horizontal

rectRange :: Functor f => Orientation -> Lens.LensLike' f Rect (Rect.Range R)
rectRange Horizontal = Rect.horizontalRange
rectRange Vertical = Rect.verticalRange

data Order = Forward | Backward
    deriving (Eq, Show, Ord, Generic)

reverseOrder :: Order -> Order
reverseOrder Forward = Backward
reverseOrder Backward = Forward

applyOrder :: Order -> (a -> a -> b) -> a -> a -> b
applyOrder Forward = id
applyOrder Backward = flip

englishName :: IsString a => Orientation -> Order -> a
englishName Horizontal Backward = "left"
englishName Horizontal Forward = "right"
englishName Vertical Backward = "up"
englishName Vertical Forward = "down"

Lens.makePrisms ''Orientation
Lens.makePrisms ''Order
