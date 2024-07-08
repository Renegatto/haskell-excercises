{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module IV_Datatypes.I_EnumerationTypes.Run where
import IV_Datatypes.I_EnumerationTypes
import Data.List (unfoldr)
import Data.Foldable (traverse_)
import Control.Concurrent (threadDelay)
import Utils.Cli qualified as Cli
import System.Console.ANSI qualified as ANSI
import qualified IV_Datatypes.I_EnumerationTypes.Solution as Solution

data Init = MkInit
  { zero :: (Int,Int)
  , start :: (Int,Int)
  , target :: (Int,Int)
  , obstacles :: [(Int,Int)]
  }

newtype Advance = MkAdvance
  ((Int,Int) ->
    Walker ->
    WalkerMessage ->
    (WalkerCommand, CurrentTask))

runPls :: IO ()
runPls = do
  ANSI.clearScreen
  drawExecutedAdvance
    (MkInit
      { zero = (0,0)
      , start = (5,5)
      , target = (7,15)
      , obstacles =
        [ (5,3)
        , (4,5)
        , (5,6)
        , (7,10)
        ]

      })
    (MkAdvance Solution.advanceToTheTarget)
    (MkWalker (5,5) 20 N Advance)
    Done

-- dumbAdvance :: Advance
-- dumbAdvance = MkAdvance \_ (MkWalker (_,y) _ _ _) _ ->
--   if y `mod` 4 /= 0 then Forward else TurnLeft

drawExecutedAdvance ::
  Init ->
  Advance ->
  Walker ->
  WalkerMessage ->
  IO ()
drawExecutedAdvance draw@(MkInit {obstacles, target}) advance walker msg = do
    drawMap draw
   -- putStr $ show $ fmap (\(MkWalker pos _ _ task,cmd,msg) -> (pos,task,cmd,msg)) $ take 7 $ drop 7 executed
    drawAdvance
      (\pos -> Cli.drawThing [(pos,"x")])
      points
    ANSI.setCursorPosition 0 0
  where
    points = (\(w,_,_) -> w)
      <$> executed
    executed = runWithAdvanceToTheTarget
      obstacles
      walker
      target
      advance
      msg
      

drawMap :: Init -> IO ()
drawMap (MkInit {zero, start, target, obstacles}) = do
  Cli.drawThing $ (,".") <$> Cli.square zero 20 20
  Cli.drawThing $ (,"ж") <$> obstacles
  Cli.drawThing $ [(target,"O"), (start,"o")]

drawAdvance :: ((Int,Int) -> IO ()) -> [Walker] -> IO ()
drawAdvance draw = traverse_ \(MkWalker pos _ _ _) -> do
  draw pos
  threadDelay 500_000 -- 0.5s

runWithAdvanceToTheTarget
  :: [(Int,Int)] -- obstacles
  -> Walker -- initial
  -> (Int,Int) -- target
  -> Advance
  -> WalkerMessage -- initial message
  -> [(Walker,WalkerCommand,WalkerMessage)]
runWithAdvanceToTheTarget obstacles walker target (MkAdvance advance) initMsg =
  unfoldr (\(walker',msg') ->
    (\(w,cmd,msg) -> ((w,cmd,msg),(w,msg))) <$> advanceToTheTargetWith
      (advance target)
      obstacles
      (\(MkWalker pos _ _ _) -> pos == target)
      walker'
      msg'
    )
    (walker,initMsg)

advanceToTheTargetWith
  :: (Walker -> WalkerMessage -> (WalkerCommand, CurrentTask))
  -> [(Int,Int)]
  -> (Walker -> Bool)
  -> Walker
  -> WalkerMessage
  -> Maybe (Walker,WalkerCommand,WalkerMessage)
advanceToTheTargetWith advance obstacles isDone walker msg
  | isDone walker = Nothing
  | otherwise = 
    let
      (msg', walker'') = evalCommand cmd
    in Just (walker'',cmd,msg')
  where
    (cmd, task') = advance walker msg
    MkWalker pos fuel facing _ = walker
    walker' = MkWalker pos fuel facing task'
    evalCommand
      | movementFuelCost <= fuel = evalAnyCommand
      | otherwise = \case
        DoNothing -> (Done,walker')
        Refuel -> (Done,MkWalker pos (fuel + fuelPerRefuel) facing task')
        _ -> (NotEnoughFuel,walker')
    evalAnyCommand = \case
      TurnLeft -> (Done,MkWalker pos (pred fuel) (left facing) task')
      TurnRight -> (Done,MkWalker pos (pred fuel) (right facing) task')
      Forward
        | pointInFront pos facing `elem` obstacles ->
          (IAmFacingObstacle,walker')
        | otherwise -> (Done,MkWalker (pointInFront pos facing) (pred fuel) facing task')
      Refuel -> (Done,MkWalker pos (fuel + fuelPerRefuel) facing task')
      DoNothing -> (Done,walker')

pointInFront :: (Int,Int) -> Facing -> (Int,Int)
pointInFront (x,y) = \case
  N -> (x,succ y)
  E -> (succ x,y)
  S -> (x,pred y)
  W -> (pred x,y)

left :: Facing -> Facing
left N = W
left W = S
left S = E
left E = N

right :: Facing -> Facing
right N = E
right E = S
right S = W
right W = N