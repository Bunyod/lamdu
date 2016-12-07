{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Lamdu.Sugar.Names.Get
    ( fromExpression, fromBody
    ) where

import           Lamdu.Prelude

import           Control.Monad.Trans.State (State, runState)
import qualified Control.Monad.Trans.State as State
import           Lamdu.Sugar.Names.CPS (CPS(..))
import           Lamdu.Sugar.Names.Walk (MonadNaming)
import qualified Lamdu.Sugar.Names.Walk as Walk
import           Lamdu.Sugar.Types

newtype Collect name (m :: * -> *) a = Collect { unCollect :: State [name] a }
    deriving (Functor, Applicative, Monad)

runCollect :: Collect name m a -> (a, [name])
runCollect = (`runState` []) . unCollect

instance Monad m => MonadNaming (Collect name m) where
    type OldName (Collect name m) = name
    type NewName (Collect name m) = ()
    type TM (Collect name m) = m
    opRun = pure $ Walk.InTransaction (return . fst . runCollect)
    opWithParamName _ = cpsTellName
    opWithLetName _ = cpsTellName
    opWithDefName = cpsTellName
    opWithTagName = cpsTellName
    opGetName _ = tellName

tellName :: Walk.NameConvertor (Collect name m)
tellName name = Collect (State.modify (name:))

cpsTellName :: Walk.CPSNameConvertor (Collect name m)
cpsTellName name = CPS $ \k -> (,) <$> tellName name <*> k

-- | Returns all the *foldable* names in the given expression
-- (excluding names hidden behind transactions)
fromExpression :: Monad m => Expression name m a -> [name]
fromExpression = snd . runCollect . Walk.toExpression

fromBody :: Monad m => Body name m expr -> [name]
fromBody = snd . runCollect . Walk.toBody pure
