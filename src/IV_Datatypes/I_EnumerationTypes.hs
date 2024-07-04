{-# LANGUAGE DerivingStrategies #-}
module IV_Datatypes.I_EnumerationTypes where

-- | A turtle, it has
data Turtle = MkTurle
  (Int,Int) -- a position on XY plane
  Fuel -- a fuel level
  Facing -- a facing

newtype Fuel = MkFuel Int
  deriving (Num, Show, Eq, Ord)

data TurtleCommand
  = TurnLeft
  | TurnRight
  | Forward
  | Refuel
  | DoNothing
  deriving (Eq, Ord, Enum, Show)

data TurtleMessage
  = IAmFacingObstacle
  | NotEnoughFuel
  | Done
  deriving (Eq, Ord, Enum, Show)
data Facing = N | E | S | W -- clockwise
  deriving (Eq, Show, Enum)

-- | How much fuel is spend for a movement, but not for refuel
movementFuelCost :: Fuel
movementFuelCost = 1

-- | How many fuel is refueled per one 'Refuel' command
fuelPerRefuel :: Fuel
fuelPerRefuel = 5

-- | Based on the current position and a message after previous step giving a command that
-- closing a turtle to the specified target.
-- Note: obstacles do not need to be resolved completely, since this is not possible.
advanceToTheTarget ::
  (Int,Int) ->
  Fuel ->
  (Int,Int) ->
  Facing ->
  TurtleMessage ->
  TurtleCommand
advanceToTheTarget targetPosition currentFuel currentPosition facing message = undefined

-- | Based on the starting point and a sequence of commands predicts
-- where the turtle will end up following them.
predictDestination ::
  Fuel ->
  (Int,Int) ->
  Facing ->
  [TurtleCommand] ->
  (Int,Int)
predictDestination startingFuel startingPosition startingFacing commands = undefined

-- | Minimizes amount of commands such that the outcome is the same
-- Requirements:
-- 1. two actions in a row such that one cancel another should not take place.
-- 2. there should be no action such that when executed does nothing
optimize :: [TurtleCommand] -> [TurtleCommand]
optimize = undefined

-- | As a turtle, given current fuel level, current position, obstacles map
-- and a command, respond with a corresponding message
-- and state after attempting to complete the command
respond ::
  Fuel ->
  (Int,Int) ->
  Facing ->
  [(Int,Int)] ->
  TurtleCommand ->
  (Fuel, (Int,Int), [TurtleMessage])
respond fuelLevel currentPosition currentFacing obstaclesMap command = undefined 
