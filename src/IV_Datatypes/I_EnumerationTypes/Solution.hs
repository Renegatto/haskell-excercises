{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
module IV_Datatypes.I_EnumerationTypes.Solution where
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import IV_Datatypes.I_EnumerationTypes (Facing(..),Fuel(..), TurtleMessage(..), TurtleCommand(..))

data Turn = Clockwise | Counterclockwise | NoTurn
  deriving Show

-- data DirectionRelative = Right | Left | Front | Back

facingSucc :: Facing -> Facing
facingSucc W = N
facingSucc x = succ x

facingPred :: Facing -> Facing
facingPred N = W
facingPred x = pred x

newtype Tag l a = MkTag a

shortest :: Eq a
  => Tag "pred" (a -> a)
  -> Tag "succ" (a -> a)
  -> Tag "target" a
  -> Tag "start" a
  -> Turn
shortest (MkTag pre) (MkTag suc) (MkTag target) (MkTag start)
  | target == start = NoTurn
  | otherwise = go start start
  where
  go prev next 
    | target == prev = Counterclockwise
    | target == next = Clockwise
    | otherwise = go (pre prev) (suc next)

turnOrMove facing target =
  case shortest
    (MkTag facingPred)
    (MkTag facingSucc)
    (MkTag @"target" target)
    (MkTag @"start" facing) of

    NoTurn -> Forward
    Clockwise -> TurnRight
    Counterclockwise -> TurnLeft

shouldMoveY y y0 = shouldMoveTo y y0 N S
shouldMoveX x x0 = shouldMoveTo x x0 E W
shouldMoveTo curr target goUp goDown = case compare curr target of
  EQ -> Nothing
  LT -> Just goUp
  GT -> Just goDown

advanceToTheTarget ::
  (Int,Int) ->
  Fuel ->
  (Int,Int) ->
  Facing ->
  TurtleMessage ->
  TurtleCommand
advanceToTheTarget _ 0 _ _ _ = Refuel
advanceToTheTarget target _ pos _ _
  | target == pos = DoNothing
advanceToTheTarget (x0,y0) _ (x,y) facing message =
  fromMaybe DoNothing $ case message of
    NotEnoughFuel -> Just Refuel
    IAmFacingObstacle
      | shouldMoveY y y0 == Just facing -> moveX
      | shouldMoveX x x0 == Just facing -> moveY
      | otherwise -> Nothing
    Done -> moveX <|> moveY
  where
    moveY = turnOrMove facing <$> shouldMoveY y y0
    moveX = turnOrMove facing <$> shouldMoveX x x0