{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module IV_Datatypes.I_EnumerationTypes.Run where
import IV_Datatypes.I_EnumerationTypes
import Data.List (unfoldr)
import Data.Foldable (traverse_)
import Control.Concurrent (threadDelay)
import Utils.Cli qualified as Cli
import System.Console.ANSI qualified as ANSI

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
    WalkerCommand)

runPls :: IO ()
runPls = do
  ANSI.clearScreen
  drawExecutedAdvance
    (MkInit
      { zero = (0,0)
      , start = (5,5)
      , target = (10,4)
      , obstacles =
        [ (5,3)
        , (4,5)
        , (7,6)
        ]

      })
    dumbAdvance
    (MkWalker (5,5) 20 N)
    Done

dumbAdvance :: Advance
dumbAdvance = MkAdvance \_ (MkWalker (_,y) _ _) _ ->
  if y `mod` 4 /= 0 then Forward else TurnLeft

drawExecutedAdvance ::
  Init ->
  Advance ->
  Walker ->
  WalkerMessage ->
  IO ()
drawExecutedAdvance draw@(MkInit {obstacles, target}) advance walker msg = do
    ANSI.saveCursor
    drawMap draw
    ANSI.restoreCursor
   -- putStr $ show $ fmap (\(MkWalker pos _ _,_,msg) -> (pos,msg)) $ take 7 executed
    drawAdvance
      (\pos -> Cli.drawThing [(pos,"x")])
      points
  where
    points = take 10 
      $ (\(w,_,_) -> w)
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
  ANSI.restoreCursor
  Cli.drawThing $ (,"ж") <$> obstacles
  ANSI.restoreCursor
  Cli.drawThing $ [(target,"O"), (start,"o")]
  ANSI.restoreCursor

drawAdvance :: ((Int,Int) -> IO ()) -> [Walker] -> IO ()
drawAdvance draw = traverse_ \(MkWalker pos _ _) -> do
  draw pos
  threadDelay 1000

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
      (\(MkWalker pos _ _) -> pos == target)
      walker'
      msg'
    )
    (walker,initMsg)

advanceToTheTargetWith
  :: (Walker -> WalkerMessage -> WalkerCommand)
  -> [(Int,Int)]
  -> (Walker -> Bool)
  -> Walker
  -> WalkerMessage
  -> Maybe (Walker,WalkerCommand,WalkerMessage)
advanceToTheTargetWith advance obstacles isDone walker msg
  | isDone walker = Nothing
  | otherwise = 
    let
      (msg', walker') = evalCommand cmd
    in Just (walker',cmd,msg')
  where
    cmd = advance walker msg
    MkWalker pos fuel facing = walker
    evalCommand
      | movementFuelCost <= fuel = evalAnyCommand
      | otherwise = \case
        DoNothing -> (Done,walker)
        Refuel -> (Done,MkWalker pos (fuel + fuelPerRefuel) facing)
        _ -> (NotEnoughFuel,walker)
    evalAnyCommand = \case
      TurnLeft -> (Done,MkWalker pos (pred fuel) (left facing))
      TurnRight -> (Done,MkWalker pos (pred fuel) (right facing))
      Forward
        | pointInFront pos facing `elem` obstacles ->
          (IAmFacingObstacle,walker)
        | otherwise -> (Done,MkWalker (pointInFront pos facing) (pred fuel) facing)
      Refuel -> (Done,MkWalker pos (fuel + fuelPerRefuel) facing)
      DoNothing -> (Done,walker)

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