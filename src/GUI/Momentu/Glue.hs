{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module GUI.Momentu.Glue
    ( Glue(..), GluesTo
    , (/|/), (/-/)
    , box, hbox, vbox
    , glueH
    , mkGlue
    , Orientation(..)
    , Poly(..), mkPoly
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Direction (Orientation(..), axis, perpendicular)
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element

import           Lamdu.Prelude

class (Glued b a ~ Glued a b) => Glue a b where
    type Glued a b
    glue :: Dir.Layout -> Orientation -> a -> b -> Glued a b

type GluesTo a b c = (Glue a b, Glue b a, Glued a b ~ c)

newtype Poly = Poly { polyGlue :: forall a b. Glue a b => a -> b -> Glued a b }

mkPoly ::
    (MonadReader env m, Dir.HasLayoutDir env) => m (Orientation -> Poly)
mkPoly =
    Lens.view Dir.layoutDir
    <&> \dir orientation -> Poly (glue dir orientation)

mkGlue ::
    (MonadReader env m, Dir.HasLayoutDir env, Glue a b) =>
    m (Orientation -> a -> b -> Glued a b)
mkGlue = mkPoly <&> (polyGlue .)

-- Horizontal glue
(/|/) ::
    (MonadReader env m, Dir.HasLayoutDir env, Glue a b) =>
    m a -> m b -> m (Glued a b)
l /|/ r = (mkGlue ?? Horizontal) <*> l <*> r

-- Vertical glue
(/-/) ::
    (MonadReader env m, Dir.HasLayoutDir env, Glue a b) =>
    m a -> m b -> m (Glued a b)
l /-/ r = (mkGlue ?? Vertical) <*> l <*> r

glueH ::
    (SizedElement a, SizedElement b) =>
    (a -> b -> c) -> Dir.Layout -> Orientation -> a -> b -> c
glueH f direction orientation v0 v1 =
    f
    (Element.pad direction v0pre v0post v0)
    (Element.pad direction v1pre v1post v1)
    where
        v0pre = 0
        v0post = v1s & perp -~ v0s ^. perp & perp %~ max 0
        v1pre = v0s & perp .~ 0
        v1post = v0s & ax .~ 0 & perp -~ v1s ^. perp & perp %~ max 0
        ax = axis orientation
        perp :: Lens' (Vector2 a) a
        perp = axis (perpendicular orientation)
        v0s = v0 ^. Element.size
        v1s = v1 ^. Element.size

box ::
    (Element a, GluesTo a a a, Dir.HasLayoutDir env, MonadReader env m) =>
    m (Orientation -> [a] -> a)
box = mkGlue <&> \g orientation -> foldr (g orientation) Element.empty

hbox ::
    (Element a, GluesTo a a a, Dir.HasLayoutDir env, MonadReader env m) =>
    m ([a] -> a)
hbox = box ?? Horizontal

vbox ::
    (Element a, GluesTo a a a, Dir.HasLayoutDir env, MonadReader env m) =>
    m ([a] -> a)
vbox = box ?? Vertical
