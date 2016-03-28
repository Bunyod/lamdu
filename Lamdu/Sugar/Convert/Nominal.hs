{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Nominal
    ( convertFromNom, convertToNom
    ) where

import           Control.Lens.Operators
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActionsWithSetToInner)
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Text as ConvertText
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.TIdG as ConvertTIdG
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Prelude.Compat

convertFromNom ::
    (MonadA m, Monoid a) => V.Nom (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertFromNom = convert BodyFromNom

convert ::
    (MonadA m, Monoid a) =>
    (Nominal Guid (ExpressionU m a) -> BodyU m b) ->
    V.Nom (Val (Input.Payload m a)) ->
    Input.Payload m b -> ConvertM m (ExpressionU m b)
convert f (V.Nom tid val) exprPl =
    Nominal
    { _nTId = ConvertTIdG.convert tid
    , _nVal = val
    }
    & traverse ConvertM.convertSubexpression
    <&> f
    >>= addActionsWithSetToInner exprPl val

convertToNom ::
    (MonadA m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertToNom nom exprPl =
    do
        ConvertText.text nom exprPl & justToLeft
        convert BodyToNom nom exprPl & lift
    & runMatcherT
