module Test.Main where

import Control.Monad.State (runState, get)
import Control.Monad.State.Trans (runStateT)
import Control.Monad.Update (Update, auditUpdate, collectUpdate, evalUpdate, execUpdate, runUpdate, getState, putAction)
import Control.Monad.UpdateState (UpdateState, runUpdateState)
import Control.Monad.UpdateStateT (UpdateStateT, runUpdateStateT)
import Control.Monad.UpdateT (UpdateT, runUpdateT)
import Data.Array (reverse)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (floor, toNumber)
import Data.Monoid.Action
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Prelude (class Eq, class Ord, class Show, Unit, bind, discard, negate, pure, ($), (*), (+), (-), (<<<), (>=))
import Test.Assert (assertEqual)

-- from https://chrispenner.ca/posts/update-monad

-- Simple type to keep track our bank balance
newtype BankBalance = BankBalance Int

derive newtype instance eqBankBalance :: Eq BankBalance
derive newtype instance ordBankBalance :: Ord BankBalance
derive newtype instance showBankBalance :: Show BankBalance

-- The three types of actions we can take on our account
data AccountAction = Deposit Int | Withdraw Int | ApplyInterest

derive instance eqAccountAction :: Eq AccountAction
derive instance ordAccountAction :: Ord AccountAction
derive instance genericAccountAction :: Generic AccountAction _

instance showAccountAction :: Show AccountAction where
  show = genericShow

-- We can apply any of our actions to our bank balance to get a new balance
processTransaction :: AccountAction -> BankBalance -> BankBalance
processTransaction (Deposit n) (BankBalance b) 
    = BankBalance (b + n)
processTransaction (Withdraw n) (BankBalance b) 
    = BankBalance (b - n)

-- This is a gross oversimplification...
-- I really hope my bank does something smarter than this
-- We (kinda sorta) add 10% interest, truncating any cents.
-- Who likes pocket-change anyways ¯\_(ツ)_/¯
processTransaction ApplyInterest (BankBalance b) 
    = BankBalance (floor $ (toNumber b * 1.1))

-- instance ActionBankBalance :: Action AccountAction BankBalance where
--   act = processTransaction

instance actionBankBalanceArray :: Action (Array AccountAction) BankBalance where
  act actions balance =
    let allTransactions :: BankBalance -> BankBalance
        allTransactions = unwrap $ foldMap (Endo <<< processTransaction) (reverse actions)
     in allTransactions balance

useATM :: Update (Array AccountAction) BankBalance Boolean
useATM = do
  putAction [Deposit 20] -- BankBalance 20
  putAction [Deposit 30] -- BankBalance 50
  putAction [ApplyInterest] -- BankBalance 55
  putAction [Withdraw 10] -- BankBalance 45
  BankBalance b <- getState
  pure $ b >= 0

useATM' :: UpdateState (Array AccountAction) BankBalance Boolean
useATM' = do
  putAction [Deposit 20] -- BankBalance 20
  putAction [Deposit 30] -- BankBalance 50
  putAction [ApplyInterest] -- BankBalance 55
  putAction [Withdraw 10] -- BankBalance 45
  BankBalance b <- getState
  pure $ b >= 0

useATMT :: UpdateT (Array AccountAction) BankBalance Effect Boolean
useATMT = do
  putAction [Deposit 20] -- BankBalance 20
  putAction [Deposit 30] -- BankBalance 50
  putAction [ApplyInterest] -- BankBalance 55
  putAction [Withdraw 10] -- BankBalance 45
  BankBalance b <- getState
  liftEffect $ logShow b
  pure $ b >= 0


useATMT' :: UpdateStateT (Array AccountAction) BankBalance Effect Boolean
useATMT' = do
  putAction [Deposit 20] -- BankBalance 20
  putAction [Deposit 30] -- BankBalance 50
  (Tuple s _ ) <- get
  liftEffect $ logShow s
  putAction [ApplyInterest] -- BankBalance 55
  putAction [Withdraw 10] -- BankBalance 45
  BankBalance b <- getState
  liftEffect $ logShow b
  pure $ b >= 0


main :: Effect Unit
main = do
  assertEqual 
    { actual: runUpdate useATM (BankBalance 0)
    , expected : 
      ( Tuple [ (Deposit 20)
              , (Deposit 30)
              , ApplyInterest
              , (Withdraw 10)
              ] true )
    }
  assertEqual 
    { actual: evalUpdate useATM (BankBalance 0)
    , expected : true
    }
  assertEqual 
    { actual: evalUpdate useATM (BankBalance (-50))
    , expected : false
    }
  assertEqual 
    { actual: execUpdate useATM (BankBalance 0)
    , expected : BankBalance 45
    }
  assertEqual 
    { actual: collectUpdate useATM (BankBalance 0)
    , expected : [ (Deposit 20)
                 , (Deposit 30)
                 , ApplyInterest
                 , (Withdraw 10)
                 ] 
    }
  assertEqual 
    { actual: auditUpdate useATM (BankBalance 0)
    , expected : Tuple (BankBalance 45) 
      ( Tuple [ (Deposit 20)
              , (Deposit 30)
              , ApplyInterest
              , (Withdraw 10)
              ] true )
    }  
  assertEqual 
    { actual: runState (runUpdateState useATM') $ Tuple [] (BankBalance 0)
    , expected : 
      ( Tuple true (Tuple [ (Deposit 20)
                 , (Deposit 30)
                 , ApplyInterest
                 , (Withdraw 10)
                 ] (BankBalance 45) ) )
    }
  res <- runUpdateT useATMT (BankBalance 0)
  assertEqual 
    { actual: res
    , expected : 
      ( Tuple [ (Deposit 20)
              , (Deposit 30)
              , ApplyInterest
              , (Withdraw 10)
              ] true )
    }
  res' <- runStateT (runUpdateStateT useATMT') $ Tuple [] (BankBalance 0)
  assertEqual 
    { actual: res
    , expected : 
      ( Tuple [ (Deposit 20)
              , (Deposit 30)
              , ApplyInterest
              , (Withdraw 10)
              ] true )
    }