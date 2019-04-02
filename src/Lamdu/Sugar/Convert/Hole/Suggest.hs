{-# LANGUAGE TupleSections, TypeFamilies #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( forType
    , termTransforms
    , termTransformsWithModify
    ) where

import           AST (monoChildren)
import           AST.Knot.Ann (Ann(..), ann, val, annotations)
import           AST.Term.Row (RowExtend(..))
import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad (mzero)
import           Control.Monad.Trans.State (StateT(..), mapStateT)
import qualified Data.List.Class as ListClass
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (Nominal)
import qualified Lamdu.Calc.Type.Nominal as Nominal
import           Lamdu.Calc.Type.Scheme (schemeType)
import           Lamdu.Infer (Context, Payload(..))
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Unify (unify)
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

type Nominals = Map T.NominalId Nominal

loadNominalsForType :: Monad m => (T.NominalId -> m (Maybe Nominal)) -> Type -> m Nominals
loadNominalsForType loadNominal typ =
    go Map.empty (typ ^. ExprLens.typeTIds . Lens.to Set.singleton)
    <&> Map.mapMaybe id
    where
        go res toLoad
            | Set.null toLoad = pure res
            | otherwise =
                do
                    nominals <- Map.fromSet loadNominal toLoad & sequenceA
                    let result = res <> nominals
                    let newTIds =
                            nominals
                            ^. Lens.traversed . Lens._Just
                            . Nominal.nomType . schemeType
                            . ExprLens.typeTIds . Lens.to Set.singleton
                            & (`Set.difference` Map.keysSet result)
                    go result newTIds

-- | These are offered in fragments (not holes). They transform a term
-- by wrapping it in a larger term where it appears once.
termTransforms ::
    Monad m =>
    (T.NominalId -> m (Maybe Nominal)) -> a ->
    Val (Payload, a) -> m (StateT Context [] (Val (Payload, a)))
termTransforms loadNominal empty src =
    loadNominalsForType loadNominal
    (src ^. ann . _1 . Infer.plType)
    <&> \nominals -> termTransformsH nominals empty src

termTransformsH ::
    Nominals -> a -> Val (Payload, a) ->
    StateT Context [] (Val (Payload, a))
termTransformsH nominals empty src =
    case srcInferPl ^. Infer.plType of
    T.TRecord composite
        | Lens.nullOf (val . V._BRecExtend) src ->
        src : (composite ^.. ExprLens.compositeFields <&> getField) & lift
        where
            getField (tag, typ) =
                V.GetField src tag
                & V.BGetField
                & Ann (Payload typ (srcInferPl ^. Infer.plScope), empty)
    _ -> termTransformsWithoutSplit nominals empty src
    where
        srcInferPl = src ^. ann . _1

prependOpt :: a -> StateT Context [] a -> StateT Context [] a
prependOpt opt = Lens._Wrapped . Lens.imapped %@~ (:) . (,) opt

termTransformsWithoutSplit ::
    Nominals -> a -> Val (Payload, a) ->
    StateT Context [] (Val (Payload, a))
termTransformsWithoutSplit nominals empty src =
    prependOpt src $
    case srcType of
    T.TInst name _params
        | Lens.has (Lens.ix name) nominals
        && Lens.nullOf V._BToNom srcVal ->
        -- TODO: Expose primitives from Infer to do this without partiality
        do
            fromNomType <- Infer.inferFromNom nominals name srcScope
            let T.TFun _ innerType = fromNomType
            V.BApp V.Apply
                { V._applyFunc = V.LFromNom name & V.BLeaf & mkRes fromNomType
                , V._applyArg = src
                } & mkRes innerType & pure
        & Infer.run
        & mapStateT
            (either (error "Infer of FromNom on non-opaque Nominal shouldn't fail") pure)
        >>= termTransformsWithoutSplit nominals empty
    T.TFun argType resType | Lens.nullOf V._BLam srcVal ->
        if Lens.has (ExprLens.valLeafs . V._LHole) arg
            then
                -- If the suggested argument has holes in it
                -- then stop suggesting there to avoid "overwhelming"..
                pure applied
            else termTransformsWithoutSplit nominals empty applied
        where
            arg =
                forTypeWithoutSplit (Payload argType srcScope)
                & annotations %~ (, empty)
            applied = V.Apply src arg & V.BApp & mkRes resType
    T.TVariant composite | Lens.nullOf V._BInject srcVal ->
        Infer.freshInferredVar srcScope "s"
        & Infer.run
        & mapStateT
            (either (error "Infer.freshInferredVar shouldn't fail") pure)
        <&>
        \dstType ->
        suggestCaseWith composite (Payload dstType srcScope)
        & annotations %~ (, empty)
        & (`V.Apply` src) & V.BApp & mkRes dstType
    _ -> mzero
    where
        srcInferPl = src ^. ann . _1
        srcType = srcInferPl ^. Infer.plType
        srcScope = srcInferPl ^. Infer.plScope
        mkRes typ = Ann (Payload typ srcScope, empty)
        srcVal = src ^. val

-- | Suggest values that fit a type, may "split" once, to suggest many
-- injects for a sum type. These are offerred in holes (not fragments).
forType :: Payload -> [Val Payload]
forType pl@(Payload (T.TVariant comp) scope) =
    case comp of
    T.RVar{} -> [V.BLeaf V.LHole]
    _ -> comp ^.. ExprLens.compositeFields <&> inject
    <&> Ann pl
    where
        inject (tag, innerTyp) =
            forTypeWithoutSplit (Payload innerTyp scope) & V.Inject tag & V.BInject
forType typ = [forTypeWithoutSplit typ]

forTypeWithoutSplit :: Payload -> Val Payload
forTypeWithoutSplit (Payload (T.TRecord composite) scope) =
    suggestRecordWith composite scope
forTypeWithoutSplit (Payload (T.TFun (T.TVariant composite) r) scope) =
    suggestCaseWith composite (Payload r scope)
forTypeWithoutSplit pl@(Payload typ scope) =
    case typ of
    T.TFun _ r ->
        -- TODO: add var to the scope?
        forTypeWithoutSplit (Payload r scope) & V.Lam "var" & V.BLam
    _ -> V.BLeaf V.LHole
    & Ann pl

suggestRecordWith :: T.Row -> Infer.Scope -> Val Payload
suggestRecordWith recordType scope =
    case recordType of
    T.RVar{} -> V.BLeaf V.LHole
    T.REmpty -> V.BLeaf V.LRecEmpty
    T.RExtend f t r ->
        RowExtend f
        (valueSimple (Payload t scope))
        (suggestRecordWith r scope)
        & V.BRecExtend
    & Ann (Payload (T.TRecord recordType) scope)

suggestCaseWith :: T.Row -> Payload -> Val Payload
suggestCaseWith variantType resultPl@(Payload resultType scope) =
    case variantType of
    T.RVar{} -> V.BLeaf V.LHole
    T.REmpty -> V.BLeaf V.LAbsurd
    T.RExtend tag fieldType rest ->
        RowExtend tag
        (valueSimple (Payload (T.TFun fieldType resultType) scope))
        (suggestCaseWith rest resultPl)
        & V.BCase
    & Ann (Payload (T.TFun (T.TVariant variantType) resultType) scope)

valueSimple :: Payload -> Val Payload
valueSimple pl@(Payload typ scope) =
    case typ of
    T.TFun _ r ->
        -- TODO: add var to the scope?
        valueSimple (Payload r scope) & V.Lam "var" & V.BLam
    _ -> V.BLeaf V.LHole
    & Ann pl

fillHoles :: a -> Val (Payload, a) -> Val (Payload, a)
fillHoles empty (Ann pl (V.BLeaf V.LHole)) =
    forTypeWithoutSplit (pl ^. _1)
    & annotations %~ (, empty)
    & ann . _2 .~ (pl ^. _2)
fillHoles empty (Ann pl (V.BApp (V.Apply func arg))) =
    -- Dont fill in holes inside apply funcs. This may create redexes..
    fillHoles empty arg & V.Apply func & V.BApp & Ann pl
fillHoles _ v@(Ann _ (V.BGetField (V.GetField (Ann _ (V.BLeaf V.LHole)) _))) =
    -- Dont fill in holes inside get-field.
    v
fillHoles empty x = x & val . monoChildren %~ fillHoles empty

-- | Transform by wrapping OR modifying a term. Used by both holes and
-- fragments to expand "seed" terms. Holes include these as results
-- whereas fragments emplace their content inside holes of these
-- results.
termTransformsWithModify ::
    ListClass.List m =>
    (T.NominalId -> StateT Context m (Maybe Nominal)) -> a -> Val (Payload, a) ->
    StateT Context m (Val (Payload, a))
termTransformsWithModify _ _ v@(Ann _ V.BLam {}) = pure v -- Avoid creating a surprise redex
termTransformsWithModify _ _ v@(Ann pl0 (V.BInject (V.Inject tag (Ann pl1 (V.BLeaf V.LHole))))) =
    -- Variant:<hole> ==> Variant.
    pure (Ann pl0 (V.BInject (V.Inject tag (Ann pl1 (V.BLeaf V.LRecEmpty)))))
    <|> pure v
termTransformsWithModify loadNominal empty x =
    case inferPl ^. Infer.plType of
    T.TVar tv
        | any (`Lens.has` x)
            [ ExprLens.valVar
            , ExprLens.valGetField . V.getFieldRecord . ExprLens.valVar
            ] ->
            -- a variable that's compatible with a function type
            pure x <|>
            do
                arg <- freshVar "af"
                res <- freshVar "af"
                let varTyp = T.TFun arg res
                unify varTyp (T.TVar tv)
                    & Infer.run & mapStateT assertSuccess
                V.BLeaf V.LHole
                    & Ann (plSameScope arg)
                    & V.Apply x & V.BApp
                    & Ann (plSameScope res)
                    & pure
        where
            assertSuccess (Left err) =
                fail $
                "Unify of a tv with function type should always succeed, but failed: " ++
                prettyShow err
            assertSuccess (Right t) = pure t
            freshVar = Infer.run . Infer.freshInferredVar (inferPl ^. Infer.plScope)
            scope = inferPl ^. Infer.plScope
            plSameScope t = (Infer.Payload t scope, empty)
    T.TRecord{} | Lens.has ExprLens.valVar x ->
        -- A "params record" (or just a let item which is a record..)
        pure x
    _ ->
        x
        & fillHoles empty
        & termTransforms loadNominal empty
        <&> mapStateT ListClass.fromList
        & join
    where
        inferPl = x ^. ann . _1
