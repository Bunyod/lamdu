{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving #-}

module Control.Monad.Once
    ( MonadOnce(..)
    , OnceT(..), _OnceT
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Dynamic (Dynamic, Typeable, toDyn, fromDynamic)
import           Data.IORef
import qualified Data.Sequence as Sequence

import           Lamdu.Prelude

class MonadOnce m where
    once :: Typeable a => m a -> m (m a)

instance MonadOnce IO where
    once a =
        newIORef Nothing
        <&>
        \r ->
        readIORef r
        >>=
        \case
        Just x -> pure x
        Nothing ->
            do
                x <- a
                x <$ writeIORef r (Just x)

newtype OnceT m a = OnceT (StateT (Sequence.Seq Dynamic) m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance (Semigroup a, Monad m) => Semigroup (OnceT m a) where
    a <> b = (<>) <$> a <*> b

Lens.makePrisms ''OnceT

instance Monad m => MonadOnce (OnceT m) where
    once (a :: OnceT m a) =
        id <<%= (|> toDyn (Nothing :: Maybe a)) <&> Sequence.length & OnceT
        <&>
        \r ->
        OnceT (Lens.use id) <&> (^?! Lens.ix r) <&> fromDynamic
        >>=
        \case
        Just (Just x) -> pure x
        Just Nothing ->
            do
                x <- a
                x <$ OnceT (Lens.ix r .= toDyn x)
        Nothing -> error "Once used incorrectly!"
