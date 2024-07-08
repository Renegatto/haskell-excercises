{-# LANGUAGE BlockArguments #-}
module Utils.Cli where
import Prelude hiding (putStrLn)
import System.Console.ANSI qualified as ANSI
import Data.Foldable (traverse_)

drawThing :: [((Int,Int),String)] -> IO ()
drawThing = traverse_ \((x,y),s) -> do
  Just (height, _) <- ANSI.getTerminalSize
  ANSI.setCursorPosition (height - y) x
  -- print height
  putStr s

lineUp :: (Int,Int) -> Int -> ([(Int,Int)],(Int,Int))
lineUp (x0,y0) len = ((x0,) <$> [min y0 end..max y0 end],(x0,end))
  where end = y0 + len

lineRight :: (Int,Int) -> Int -> ([(Int,Int)],(Int,Int))
lineRight (x0,y0) len = ((,y0) <$> [min x0 end..max x0 end],(end,y0))
  where end = x0 + len

square :: (Int,Int) -> Int -> Int -> [(Int,Int)]
square bl height width =
  let
    (left, tl) = lineUp bl height
    (right, _) = lineUp br height
    (bot, br) = lineRight bl width
    (top, _) = lineRight tl width
  in left <> right <> bot <> top

squareToDraw = (,"o") <$> square (0,0) 5 10

obstacles = [(3,2),(1,5),(3,3),(2,1),(3,4)]

draw2 :: IO ()
draw2 = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  putStr "O"
  drawThing squareToDraw
  -- drawThing
  --   [ ((0,0),"00")
  --   , ((10,0),"10")
  --   , ((0,10),"01")
  --   , ((10,10),"11")
  --   ]

draw :: IO ()
draw = do
  Just (_,y) <- ANSI.getTerminalSize
  ANSI.saveCursor
  _ <- ANSI.getCursorPosition
  ANSI.cursorUp 1
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  ANSI.cursorUp 1
  print =<< ANSI.getCursorPosition
  ANSI.clearScreen
  ANSI.restoreCursor
  drawThing squareToDraw
  ANSI.restoreCursor
  drawThing [((0,0),("x"))]
  ANSI.restoreCursor
  drawThing $ (,"ж") <$> obstacles
  ANSI.setCursorPosition y 0