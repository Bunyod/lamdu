module Lamdu.Sugar.Convert.Nominal
    ( convertFromNom, convertToNom
    ) where

import           AST (Tree, Ann)
import           AST.Term.Nominal (ToNom(..))
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import qualified Lamdu.Sugar.Convert.Text as ConvertText
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertFromNom ::
    (Monad m, Monoid a) =>
    T.NominalId -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertFromNom tid pl =
    ConvertTId.convert tid <&> BodyFromNom
    >>= addActions [] pl

convertToNom ::
    (Monad m, Monoid a) =>
    Tree (ToNom NominalId V.Term) (Ann (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertToNom nom@(ToNom tid x) pl =
    do
        ConvertText.text nom pl & justToLeft
        ToNominal
            <$> ConvertTId.convert tid
            <*> ConvertBinder.convertBinder x
            <&> BodyToNom
            >>= addActions [x] pl
            & lift
    & runMatcherT
