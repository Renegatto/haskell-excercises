module IV_Datatypes.II_ProductAndSumOfProductTypes where

import IV_Datatypes.I_EnumerationTypes qualified as Walker


{-
Define a data type Turtle, that can be either Walker that can only walk,
or Breaker, that can also destroy obstacles.
Every Breaker is also a Walker, but not the other way around.

Breaker can have different tools for destroying:
- A pickaxe - the tool that have estimated durability of 53.7,
  which require 5 units of fuel for a single use.
- A hammer - the tool that have estimated durability of 205.256,
  which require 24 units of fuel for a single use.

Durability drops down when being used. How much it drops depends on the obstacle being destroyed.
-}
data Turtle -- complete this

{- Breaker have some additional messages over walker
- that it's tool has broken
- that it can not destroy the block because it's tool is broken
- that it can not destroy the block because it has not enough fuel
-}
data TurleMessage -- complete this

-- Breaker can perform only one extra command - it can destoy the block in front of it
data TurtleCommand -- complete this

data TurtleError
  = UnsupportedMessage TurleMessage

-- | Based on the current position and a message after previous step giving a command that
-- closing a Walker to the specified target.
-- Note: obstacles do not need to be resolved completely, since this is not possible.
advanceToTheTarget ::
  (Int,Int) ->
  Turtle ->
  TurleMessage ->
  Either TurtleError TurtleCommand
advanceToTheTarget targetPosition turtle message = undefined
