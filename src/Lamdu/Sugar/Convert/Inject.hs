module Lamdu.Sugar.Convert.Inject
    ( convert
    ) where

import qualified Data.Property as Property
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Tag (convertTag)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert ::
    (Monad m, Monoid a) =>
    V.Inject (Val (Input.Payload m a)) -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convert (V.Inject tag injected) exprPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let typeProtect = protectedSetToVal (exprPl ^. Input.stored) valI
        injectedS <- ConvertM.convertSubexpression injected
        let toNullary =
                do
                    V.BLeaf V.LRecEmpty & ExprIRef.newValBody
                        <&> V.Inject tag <&> V.BInject
                        >>= ExprIRef.writeValBody valI
                    typeProtect <&> EntityId.ofValI
        let inj =
                case injectedS of
                Expression pl
                    (BodyRecord
                     (Composite []
                      (ClosedComposite closedCompositeActions) addItem)) ->
                    NullaryVal pl closedCompositeActions addItem
                    & InjectNullary
                _ ->
                    injectedS
                    & body . _BodyHole . holeMDelete ?~ toNullary
                    & InjectVal
        let setTag newTag =
                do
                    V.Inject newTag injectedI & V.BInject & ExprIRef.writeValBody valI
                    void typeProtect
        convertTag tag nameWithoutContext mempty (EntityId.ofTag entityId) setTag
            <&> (`Inject` inj) <&> BodyInject
            >>= addActions [] exprPl
    where
        entityId = exprPl ^. Input.entityId
        valI = exprPl ^. Input.stored . Property.pVal
        injectedI = injected ^. Val.payload . Input.stored . Property.pVal
