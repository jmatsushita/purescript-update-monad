module Control.Monad.UpdateStateT where
  
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, pure, ($), (<$>), (<>), (>>=)) 
import Data.Tuple (Tuple(..), snd)
import Control.Monad.State.Trans (class MonadState, StateT(..), get, modify_)
import Effect.Class (class MonadEffect, liftEffect)

import Control.Monad.Update (class MonadUpdate)
import Data.Monoid.Action (class Action, act)

newtype UpdateStateT w s m a = UpdateStateT (StateT (Tuple w s) m a)

runUpdateStateT :: forall w s m a . UpdateStateT w s m a -> (StateT (Tuple w s) m a)
runUpdateStateT (UpdateStateT s) = s

derive newtype instance functorUpdateStateT :: Monad m => Functor (UpdateStateT w s m)
derive newtype instance applyUpdateStateT :: Monad m => Apply (UpdateStateT w s m)
derive newtype instance applicativeUpdateStateT :: Monad m => Applicative (UpdateStateT w s m)
derive newtype instance bindUpdateStateT :: Monad m => Bind (UpdateStateT w s m)
derive newtype instance monadUpdateStateT :: Monad m => Monad (UpdateStateT w s m)


instance monadEffectUpdateStateT :: (MonadEffect m, Action w s) => MonadEffect (UpdateStateT w s m) where
  liftEffect m = UpdateStateT $ StateT $ \s -> liftEffect m >>= \a -> pure (Tuple a s)


instance stateUpdateStateT :: Monad m => MonadState (Tuple w s) (UpdateStateT w s m) where
  -- state :: forall a. (s -> (Tuple a s)) -> m a
  -- state :: forall a. ((Tuple w s) -> (Tuple a (Tuple w s))) -> UpdateState w s a
  -- state :: forall a
  --       .              ((Tuple w s) ->          (Tuple a (Tuple w s))) 
  --       -> UpdateState ((Tuple w s) -> Identity (Tuple a (Tuple w s)))
  state f = UpdateStateT $ StateT (\s -> pure $ (f s))
    -- where 
    --   tweak (Tuple a s) w = Tuple a (Tuple w s)
  -- state f = UpdateState $ StateT (\s -> Identity $ f s)


-- g :: s -> Identity (Tuple a s)
-- g :: (Tuple w s) -> Identity (Tuple a (Tuple w s))
-- f :: s -> Tuple a s
-- f :: (Tuple w s) -> Tuple a (Tuple w s)

-- data Update w s a = Update (s -> (Tuple w a))
-- data State s a    = State  (s -> (Tuple s a))

-- get :: forall w s a. Update w s a
-- get = state \s -> Tuple w s
--   where 
--     state f = Update f

instance monadUpdateUpdateStateT :: (Action w s, Monad m) => MonadUpdate (UpdateStateT w s m) w s where
  -- putAction :: w -> m Unit
  -- modify_ :: forall s m. MonadState s m => (s -> s) -> m Unit
  -- putAction w' = modify_ (\(Tuple w s) -> Tuple (w <> w') (act w' s))
  putAction w' = modify_ (\(Tuple w s) -> Tuple (w <> w') (act w' s))
  -- getState :: m s
  getState = snd <$> get
  