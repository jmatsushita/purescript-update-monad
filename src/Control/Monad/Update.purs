module Control.Monad.Update where

import Control.Monad.State (State, StateT(..), get, modify_)
import Control.Monad.State.Class (class MonadState)
import Data.Identity (Identity(..))
import Data.Monoid.Action (class Action, act)
import Data.Tuple (Tuple(..), snd, fst)
import Data.Tuple.Nested ((/\), type (/\))
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, mempty, unit, ($), (*>), (<$>), (<*>), (<>))

-- https://chrispenner.ca/posts/update-monad
-- https://hashanp.xyz/posts/update.html

data Update w s a = Update (s -> (Tuple w a))
 
runUpdate :: forall w s a. Update w s a -> (s -> (Tuple w a))
runUpdate (Update f) = f

derive instance functorUpdate :: Functor (Update w s)

instance applicativeUpdate :: Action w s => Applicative (Update w s) where
  pure a = Update $ \_ -> (Tuple mempty a)

instance applyUpdate :: Action w s => Apply (Update w s) where
  apply (Update u) (Update t) =
    Update $ \s
      -- Run the first 'Update' with the initial state 
      -- and get the monoidal action and the function out
     ->
      let (Tuple w f) = u s
      -- Run the second 'Update' with a state which has been altered by
      -- the first action to get the 'a' and another action
          (Tuple w' a) = t (act w s)
      -- Combine the actions together and run the function
       in Tuple (w' <> w) (f a)

instance bindUpdate :: Action w s => Bind (Update w s) where
  bind (Update u) f =
    Update $ \s
     ->
      let (Tuple w a) = u s
      -- Run the given function over our resulting value to get our next Update
          Update t = f a
      -- Run our new 'Update' over the altered state
          (Tuple w' a') = t (act w s)
      -- Combine the actions together and return the result
       in Tuple (w <> w') a'

instance monadUpdate :: Action w s => Monad (Update w s)

class (Action w s, Monad m) <= MonadUpdate m w s | m -> s , m -> w
      -- Because each of our methods only uses p OR m but not both 
      -- we use functional dependencies to assert to the type system that 
      -- both s and p are determined by 'm'; this helps GHC be confident
      -- that we can't end up in spots where types could be ambiguous.
  where
    putAction :: w -> m Unit
    getState :: m s

instance monadUpdateUpdate :: Action w s => MonadUpdate (Update w s) w s where
  putAction w = Update $ \_ -> Tuple w unit
  getState = Update $ \s -> Tuple mempty s

evalUpdate :: forall w s a. Action w s => Update w s a -> s -> a
evalUpdate u s = snd $ runUpdate u s

execUpdate :: forall w s a. Action w s => Update w s a -> s -> s
execUpdate u s = snd $ runUpdate (u *> getState) s

collectUpdate :: forall w s a. Action w s => Update w s a -> s -> w
collectUpdate u s = fst $ runUpdate u s

auditUpdate :: forall w s a. Action w s => Update w s a -> s -> (s /\ w /\ a)
auditUpdate u s =
  let (w /\ (a /\ s)) = runUpdate ((/\) <$> u <*> getState) s
   in (s /\ w /\ a)

-- Stuck with MonadState based implementation
-- newtype UpdateState m s a = UpdateState (State (Tuple m s) a)

-- runUpdateState :: forall m s a . UpdateState m s a -> (State (Tuple m s) a)
-- runUpdateState (UpdateState s) = s

-- derive newtype instance functorUpdateState :: Functor (UpdateState p s)
-- derive newtype instance monadUpdateState :: Monad (UpdateState p s)

-- instance monadStateUpdateState :: MonadState s (UpdateState m s) where
--   -- state :: forall a. (s -> (Tuple a s)) -> m a
--   state f = state (\s -> Tuple (f s)) 

-- instance monadUpdatewithState :: Action m s => MonadUpdate (UpdateState m s) m s where
--   putAction p' = modify_ (\(Tuple p s) -> Tuple (p <> p') (act p' s))
--   getState = snd <$> get


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
  