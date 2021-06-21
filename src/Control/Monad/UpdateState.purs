module Control.Monad.UpdateState where

import Control.Monad.State (State, StateT(..), get, modify_)
import Control.Monad.State.Class (class MonadState)
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..), snd)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, ($), (<$>), (<>))

import Data.Monoid.Action (class Action, act)
import Control.Monad.Update (class MonadUpdate)

-- data State s a    = State  (s -> (Tuple a s))
-- type State s = StateT s Identity
-- newtype StateT s m a = StateT (s -> m (Tuple a s))
-- newtype UpdateState w s a = UpdateState (StateT (Tuple w s) Identity)
-- newtype UpdateState w s a = UpdateState ((Tuple w s) -> Identity (Tuple a (Tuple w s)))
-- data State s a    = State  (s -> (Tuple a s))
-- type State s = StateT s Identity
-- newtype StateT s m a = StateT (s -> m (Tuple a s))
-- newtype UpdateState w s a = UpdateState (StateT (Tuple w s) Identity)
-- newtype UpdateState w s a = UpdateState ((Tuple w s) -> Identity (Tuple a (Tuple w s)))
newtype UpdateState w s a = UpdateState (State (Tuple w s) a)

runUpdateState :: forall m s a . UpdateState m s a -> (State (Tuple m s) a)
runUpdateState (UpdateState s) = s

derive newtype instance functorUpdateState :: Functor (UpdateState w s)
derive newtype instance applyUpdateState :: Apply (UpdateState w s)
derive newtype instance applicativeUpdateState :: Applicative (UpdateState w s)
derive newtype instance bindUpdateState :: Bind (UpdateState w s)
derive newtype instance monadUpdateState :: Monad (UpdateState w s)

instance stateUpdateState :: MonadState (Tuple w s) (UpdateState w s) where
  -- state :: forall a. (s -> (Tuple a s)) -> m a
  -- state :: forall a. ((Tuple w s) -> (Tuple a (Tuple w s))) -> UpdateState w s a
  -- state :: forall a
  --       .              ((Tuple w s) ->          (Tuple a (Tuple w s))) 
  --       -> UpdateState ((Tuple w s) -> Identity (Tuple a (Tuple w s)))
  state f = UpdateState $ StateT (\s -> Identity $ (f s))

-- data Update w s a = Update (s -> (Tuple w a))
-- data State s a    = State  (s -> (Tuple s a))

instance monadUpdatewithState :: Action w s => MonadUpdate (UpdateState w s) w s where
  -- putAction :: w -> m Unit
  -- modify_ :: forall s m. MonadState s m => (s -> s) -> m Unit
  -- putAction w' = modify_ (\(Tuple w s) -> Tuple (w <> w') (act w' s))
  putAction w' = modify_ (\(Tuple w s) -> Tuple (w <> w') (act w' s))
  -- getState :: m s
  getState = snd <$> get
  
