{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module IV_Datatypes.I_EnumerationTypes.Solution where
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import IV_Datatypes.I_EnumerationTypes (Facing(..),Fuel(..), WalkerMessage(..), WalkerCommand(..), Walker (MkWalker), CurrentTask (AvoidObstacle, Advance))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor ((<&>))

data And a b = First a | Second b | Both a b
  deriving Functor

getFirst :: And a b -> Maybe a
getFirst = \case
  First a -> Just a
  Both a _ -> Just a
  _ -> Nothing
getSecond :: And a b -> Maybe b
getSecond = \case
  Second b -> Just b
  Both _ b -> Just b
  _ -> Nothing

instance Bifunctor And where
  bimap f g (Both a b) = Both (f a) (g b)
  bimap f _ (First a) = First (f a)
  bimap _ g (Second b) = Second (g b)

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

data Axis = X | Y
  deriving Eq

axisOf :: Facing -> Axis
axisOf N = Y
axisOf S = Y
axisOf W = X
axisOf E = X
-- pickClosestMovementDir
--   :: Tag "targetAt" Facing
--   -> Tag "facing" Facing
--   -> (Axis, Maybe Turn)
-- pickClosestMovementDir
--   (MkTag targetAt)
--   (MkTag facing) =
--   turnOrMove targetAt facing

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

turnOrMove :: Facing -> Facing -> WalkerCommand
turnOrMove facing target =
  case shortest
    (MkTag facingPred)
    (MkTag facingSucc)
    (MkTag @"target" target)
    (MkTag @"start" facing) of

    NoTurn -> Forward
    Clockwise -> TurnRight
    Counterclockwise -> TurnLeft

shouldMove :: Ord n => (n,n) -> (n,n) -> Maybe (And Facing Facing)
shouldMove (x,y) (x0,y0) =
  case (shouldMoveX x x0, shouldMoveY y y0) of
    (Just goX, Just goY) -> Just $ Both goX goY
    (Just goX, Nothing) -> Just $ First goX
    (Nothing, Just goY) -> Just $ Second goY
    (Nothing,Nothing) -> Nothing


shouldMoveY y y0 = shouldMoveTo y y0 N S
shouldMoveX x x0 = shouldMoveTo x x0 E W
shouldMoveTo curr target goUp goDown = case compare curr target of
  EQ -> Nothing
  LT -> Just goUp
  GT -> Just goDown



advanceToTheTarget ::
  (Int,Int) ->
  Walker ->
  WalkerMessage ->
  (WalkerCommand, CurrentTask)
advanceToTheTarget _ (MkWalker _ 0 _ task) _ = (Refuel,task)
advanceToTheTarget target (MkWalker pos _ _ task) _
  | target == pos = (DoNothing,task)
advanceToTheTarget (x0,y0) (MkWalker (x,y) _ facing task) message =
  case message of
    NotEnoughFuel -> (Refuel,task)
    IAmFacingObstacle
      | axisOf facing == Y
      , Just moveX <- getFirst =<< move -> (moveX,AvoidObstacle)
      | axisOf facing == X
      , Just moveY <- getSecond =<< move -> (moveY,AvoidObstacle)
      | otherwise -> (TurnRight, AvoidObstacle)
      
      -- | Just onX <- getFirst shouldMoveTo
      -- , onY == facing
      -- , Just goY <- moveY
      -- -> (goY, AvoidObstacle)
      -- | Just onX <- shouldMoveX x x0
      -- , onX == facing
      -- , Just goY <- moveY
      -- -> (goY, AvoidObstacle)
      -- | otherwise -> (TurnLeft, AvoidObstacle)
    Done -> case task of
      AvoidObstacle -> (Forward,Advance)
      Advance
        | Just shortestWay <- pickMovement
        -> (shortestWay,Advance)
        | otherwise -> (DoNothing,task) 
  where
    pickMovement = move <&> \case
      Both Forward _ -> Forward
      Both _ Forward -> Forward
      First goX -> goX
      Second goY -> goY
      Both a _ -> a

    move = bimap
      (turnOrMove facing)
      (turnOrMove facing)
      <$> shouldMoveTo
    shouldMoveTo = shouldMove (x,y) (x0,y0)
