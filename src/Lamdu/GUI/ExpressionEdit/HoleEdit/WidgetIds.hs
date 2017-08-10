-- | The widget ids of exposed hole components
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds
    ( WidgetIds(..), make
    ) where

import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.EntityId (EntityId)

data WidgetIds = WidgetIds
    { hidHole :: Widget.Id
    , hidClosedSearchArea :: Widget.Id
    , hidOpen :: Widget.Id
    , hidOpenSearchTerm :: Widget.Id
    , hidResultsPrefix :: Widget.Id
    } deriving Show

make :: EntityId -> WidgetIds
make entityId = WidgetIds
    { hidHole = holeId
    , hidClosedSearchArea = Widget.joinId holeId ["SearchArea", "SearchTerm"]
    , hidOpen             = Widget.joinId holeId ["SearchArea", "Open"]
    , hidOpenSearchTerm   = Widget.joinId holeId ["SearchArea", "Open", "SearchTerm"]
    , hidResultsPrefix    = Widget.joinId holeId ["SearchArea", "Open", "Results"]
    }
    where
        holeId = WidgetIds.fromEntityId entityId
