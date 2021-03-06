-- TODO:
-- The makeHTraversableAndBases calls below generate redundant constraints.
-- hypertypes's TH needs to support generating instances for a whole group,
-- so that it knows which other instances it will create.
{-# OPTIONS -Wno-redundant-constraints #-}

{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses, UndecidableInstances, DataKinds, GADTs, ConstraintKinds, FlexibleInstances #-}
module Lamdu.Sugar.Types.Expression
    ( Expr, Body
    , Term(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyGetVar, _BodyGetField, _BodyInject, _BodyHole
        , _BodyLiteral, _BodyCase, _BodyRecord, _BodyFragment
        , _BodyFromNom, _BodyToNom, _BodyIfElse
    , AnnotatedArg(..), aaTag, aaExpr
    , LabeledApply(..), aFunc, aSpecialArgs, aAnnotatedArgs, aPunnedArgs
    , App(..), appFunc, appArg
    , Fragment(..), fExpr, fHeal, fTypeMismatch, fOptions
    , Lambda(..), lamFunc, lamMode, lamApplyLimit
    , InjectContent(..), _InjectVal, _InjectNullary
    , Inject(..), iTag, iContent
    , GetField(..), gfRecord, gfTag
    , Nominal(..), nTId, nVal
    -- Binders
    , Let(..), lValue, lName, lUsages, lDelete, lBody, lVarInfo
    , Meta.SpecialArgs(..), Meta._Verbose, Meta._Operator
    , Meta.DefinitionState(..)
    , BinderParamScopeId(..), bParamScopeId
    , Binder(..), _BinderLet, _BinderTerm
    , Function(..)
        , fChosenScopeProp, fParams, fBody
        , fAddFirstParam, fBodyScopes
    , AssignPlain(..), apAddFirstParam, apBody
    , Assignment(..), _BodyFunction, _BodyPlain
    -- Holes
    , HoleOption(..), hoEntityId, hoSugaredBaseExpr, hoResults
    , Hole(..), holeOptions, holeMDelete
    , HoleResult(..), holeResultConverted, holeResultPick
    -- If/else
    , IfElse(..), iIf, iThen, iElse
    , Else(..), _SimpleElse, _ElseIf
    -- Record & Cases
    , Composite(..), cItems, cPunnedItems, cAddItem, cTail
    , CompositeItem(..), ciDelete, ciTag, ciExpr
    , CompositeTail(..), _OpenComposite, _ClosedComposite
    , Case(..), cKind, cBody
    , CaseArg(..), caVal, caToLambdaCase
    , CaseKind(..), _LambdaCase, _CaseWithArg
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Data.Kind (Constraint)
import           Data.Property (Property)
import           Hyper
import           Hyper.Type.AST.App (App(..), appFunc, appArg)
import           Lamdu.Data.Anchors (BinderParamScopeId(..), bParamScopeId)
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderVarRef, BinderMode)
import           Lamdu.Sugar.Types.Parts
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

type Expr e name i o a = Annotated (Payload name i o a) # e name i o
type Body e name i o a = e name i o # Annotated (Payload name i o a)

data AnnotatedArg name i o k = AnnotatedArg
    { _aaTag :: Tag name
    , _aaExpr :: k :# Term name i o
    } deriving Generic

-- TODO: func + specialArgs into a single sum type so that field order
-- matches gui order, no need for special traversal code
data LabeledApply name i o k = LabeledApply
    { _aFunc :: k :# Lens.Const (BinderVarRef name o)
    , _aSpecialArgs :: Meta.SpecialArgs (k :# Term name i o)
    , _aAnnotatedArgs :: [AnnotatedArg name i o k]
    , _aPunnedArgs :: [k :# Lens.Const (GetVar name o)]
    } deriving Generic

data InjectContent name i o k
    = InjectNullary (k :# Lens.Const (NullaryVal name i o))
    | InjectVal (k :# Term name i o)
    deriving Generic

data Inject name i o f = Inject
    { _iTag :: TagRef name i o
    , _iContent :: InjectContent name i o f
    } deriving Generic

data GetField name i o k = GetField
    { _gfRecord :: k :# Term name i o
    , _gfTag :: TagRef name i o
    } deriving Generic

data Lambda name i o f = Lambda
    { _lamMode :: BinderMode
    , _lamApplyLimit :: FuncApplyLimit
    , _lamFunc :: Function name i o f
    } deriving Generic

-- | An expression marked for transformation.
-- Holds an expression to be transformed but acts like a hole.
data Fragment name i o k = Fragment
    { _fExpr :: k :# Term name i o
    , _fHeal :: o EntityId
    , _fTypeMismatch :: Maybe (Annotated EntityId # Type name)
    , _fOptions :: i [HoleOption name i o]
    } deriving Generic

data HoleResult name i o = HoleResult
    { _holeResultConverted :: Expr Binder name i o ()
    , _holeResultPick :: o ()
    } deriving Generic

data HoleOption name i o = HoleOption
    { _hoEntityId :: EntityId
    , _hoSugaredBaseExpr :: i (Expr Binder name i o ())
    , -- A group in the hole results based on this option
      _hoResults :: ListT i (HoleResultScore, i (HoleResult name i o))
    } deriving Generic

data Hole name i o = Hole
    { _holeOptions :: i [HoleOption name i o]
        -- outer "i" here is used to read index of globals
        -- inner "i" is used to type-check/sugar every val in the option
      -- TODO: Lifter from i to o?
    , -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      _holeMDelete :: Maybe (o EntityId)
    } deriving Generic

data Else name i o f
    = SimpleElse (Term name i o f)
    | ElseIf (IfElse name i o f)
    deriving Generic

data IfElse name i o k = IfElse
    { _iIf :: k :# Term name i o
    , _iThen :: k :# Term name i o
    , _iElse :: k :# Else name i o
    } deriving Generic

data CompositeItem name i o k = CompositeItem
    { _ciDelete :: o EntityId
    , _ciTag :: TagRef name i o
    , _ciExpr :: k :# Term name i o
    } deriving Generic

data CompositeTail name i o k
    = OpenComposite (OpenCompositeActions o) (k :# Term name i o)
    | ClosedComposite (ClosedCompositeActions o)
    deriving Generic

data Composite name i o k = Composite
    { _cItems :: [CompositeItem name i o k]
    , -- Punned items are like Haskell's NamedFieldPuns
      _cPunnedItems :: [k :# Lens.Const (GetVar name o)]
    , _cTail :: CompositeTail name i o k
    , _cAddItem :: TagReplace name i o EntityId
    } deriving Generic

data CaseArg name i o k = CaseArg
    { _caVal :: k :# Term name i o
    , _caToLambdaCase :: o EntityId
    } deriving Generic

data CaseKind name i o k
    = LambdaCase
    | CaseWithArg (CaseArg name i o k)
    deriving Generic

data Case name i o k = Case
    { _cKind :: CaseKind name i o k
    , _cBody :: Composite name i o k
    } deriving Generic

data Nominal name i o k = Nominal
    { _nTId :: TId name
    , _nVal :: k :# Binder name i o
    } deriving Generic

data Term name i o k
    = BodyLam (Lambda name i o k)
    | BodySimpleApply (App (Term name i o) k)
    | BodyLabeledApply (LabeledApply name i o k)
    | BodyHole (Hole name i o)
    | BodyLiteral (Literal (Property o))
    | BodyRecord (Composite name i o k)
    | BodyGetField (GetField name i o k)
    | BodyCase (Case name i o k)
    | BodyIfElse (IfElse name i o k)
    | BodyInject (Inject name i o k)
    | BodyGetVar (GetVar name o)
    | BodyToNom (Nominal name i o k)
    | BodyFromNom (TId name)
    | BodyFragment (Fragment name i o k)
    | BodyPlaceHolder -- Used for hole results, shown as "★"
    deriving Generic

data Let name i o k = Let
    { _lValue :: k :# Assignment name i o -- "let foo = [[bar]] in x"
    , _lVarInfo :: VarInfo
    , _lUsages :: [EntityId]
    , _lName :: TagRef name i o -- let [[foo]] = bar in x
    , _lDelete :: o ()
    , _lBody :: k :# Binder name i o -- "let foo = bar in [[x]]"
    } deriving Generic

-- An expression with 0 or more let items,
-- Appear in a:
-- * Function: "\x -> [[THIS]]"
-- * ToNom: "«X [[THIS]]"
-- * Definition or let item value: "x = [[THIS]]"
-- * Let-item/redex: "let x = y in [[THIS]]"
data Binder name i o f
    = BinderLet (Let name i o f)
    | BinderTerm (Term name i o f)
    deriving Generic

data Function name i o k = Function
    { _fChosenScopeProp :: i (Property o (Maybe BinderParamScopeId))
    , _fParams :: BinderParams name i o
    , _fBody :: k :# Binder name i o
    , _fAddFirstParam :: AddFirstParam name i o
    , -- The scope inside a lambda
      _fBodyScopes :: ParamScopes
    } deriving Generic

data AssignPlain name i o f = AssignPlain
    { _apAddFirstParam :: AddFirstParam name i o
    , _apBody :: Binder name i o f
    } deriving Generic

data Assignment name i o f
    = BodyFunction (Function name i o f)
    | BodyPlain (AssignPlain name i o f)
    deriving Generic

Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''AssignPlain
Lens.makeLenses ''Case
Lens.makeLenses ''CaseArg
Lens.makePrisms ''CaseKind
Lens.makeLenses ''Composite
Lens.makeLenses ''CompositeItem
Lens.makePrisms ''CompositeTail
Lens.makeLenses ''Fragment
Lens.makeLenses ''Function
Lens.makeLenses ''GetField
Lens.makeLenses ''Hole
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
Lens.makeLenses ''IfElse
Lens.makeLenses ''Inject
Lens.makeLenses ''LabeledApply
Lens.makeLenses ''Lambda
Lens.makeLenses ''Let
Lens.makeLenses ''Nominal
Lens.makePrisms ''Assignment
Lens.makePrisms ''Binder
Lens.makePrisms ''Else
Lens.makePrisms ''InjectContent
Lens.makePrisms ''Term

traverse makeHTraversableAndBases
    [ ''AnnotatedArg, ''Assignment, ''AssignPlain, ''Binder, ''Case, ''CaseArg
    , ''CaseKind, ''Composite, ''CompositeItem, ''CompositeTail, ''Else
    , ''Fragment, ''Function, ''GetField, ''IfElse, ''Inject, ''InjectContent
    , ''LabeledApply, ''Lambda, ''Let, ''Nominal, ''Term
    ] <&> concat

-- TODO: Replace boilerplate below with TH

instance RNodes (Assignment name i o)
instance RNodes (Binder name i o)
instance RNodes (Else name i o)
instance RNodes (Function name i o)
instance RNodes (Term name i o)

type Dep c name i o =
    ( (c (Assignment name i o) :: Constraint)
    , c (Binder name i o)
    , c (Const (BinderVarRef name o))
    , c (Const (NullaryVal name i o))
    , c (Const (GetVar name o))
    , c (Else name i o)
    , c (Term name i o)
    )

instance Dep c name i o => Recursively c (Assignment name i o)
instance Dep c name i o => Recursively c (Binder name i o)
instance Dep c name i o => Recursively c (Else name i o)
instance Dep c name i o => Recursively c (Term name i o)

instance (Dep c name i o, c (Function name i o)) => Recursively c (Function name i o)

instance RTraversable (Assignment name i o)
instance RTraversable (Binder name i o)
instance RTraversable (Else name i o)
instance RTraversable (Term name i o)
