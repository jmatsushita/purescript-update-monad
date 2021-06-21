{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.UpdateT where

import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, bind, mempty, pure, unit, ($), (*>), (<$>), (<*>), (<>), (>>=))
import Effect.Class (class MonadEffect, liftEffect)

import Control.Monad.Update (class MonadUpdate, getState)
import Data.Monoid.Action (class Action, act)

import Data.Tuple.Nested (type (/\), (/\))
import Data.Tuple (fst, snd)

-- class (Action w s, Monad m) <=
--       MonadUpdate m w s
--   | m -> s
--   , m -> w
--   where
--   putAction :: w -> m Unit
--   getState :: m s

data UpdateT w s m a = UpdateT (s -> m (w /\ a))
runUpdateT :: forall w s m a. UpdateT w s m a -> (s -> m (w /\ a))
runUpdateT (UpdateT f) = f

derive instance functorUpdateT :: (Monad m) => Functor (UpdateT w s m)

instance applicativeUpdateT :: (Action w s, Monad m) => Applicative (UpdateT w s m) where
  pure a = UpdateT $ \_ -> pure (mempty /\ a)

instance applyUpdateT :: (Action w s, Monad m) => Apply (UpdateT w s m) where
  apply (UpdateT u) (UpdateT t) =
    UpdateT $ \s
     -> do
      (w /\ f) <- u s
      (w' /\ a) <- t (act w s)
      pure $ (w' <> w) /\ (f a)

instance bindUpdateT :: (Action w s, Monad m) => Bind (UpdateT w s m) where
  bind (UpdateT u) f =
    UpdateT $ \s -> do
      (w /\ a) <- u s
      let UpdateT fs = f a
      (w' /\ a') <- fs (act w s)
      pure $ (w <> w') /\ a'

instance monadUpdateT :: (Action w s, Monad m) => Monad (UpdateT w s m)


instance monadUpdateUpdateT :: (Monad m, Action w s) => MonadUpdate (UpdateT w s m) w s where
  putAction w = UpdateT $ \_ -> pure (w /\ unit)
  getState = UpdateT $ \s -> pure (mempty /\ s)

instance monadEffectUpdateT :: (MonadEffect m, Action w s) => MonadEffect (UpdateT w s m) where
  liftEffect m = UpdateT $ \_ -> liftEffect m >>= \x -> pure (mempty /\ x)

evalUpdateT :: forall w s m a. Action w s => Monad m => UpdateT w s m a -> s -> m a
evalUpdateT u s = snd <$> runUpdateT u s

execUpdateT :: forall w s m a. Action w s => Monad m => UpdateT w s m a -> s -> m s
execUpdateT u s = snd <$> runUpdateT (u *> getState) s

collectUpdateT :: forall w s m a. Action w s => Monad m => UpdateT w s m a -> s -> m w
collectUpdateT u s = fst <$> runUpdateT u s

auditUpdateT 
  :: forall w s m a
   . Monad m
  => Action w s 
  => UpdateT w s m a -> s -> m (s /\ w /\ a)
auditUpdateT u s = do
  (p /\ (a /\ s')) <- runUpdateT ((/\) <$> u <*> getState) s
  pure (s' /\ p /\ a)

