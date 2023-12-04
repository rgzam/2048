-- Import necessary modules
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.List (transpose)

-- Define the game state
data GameState = GameState
  { board :: [[Int]]
  , score :: Int
  , gen   :: StdGen
  }

-- Initial game state with a specified grid size
initialState :: Int -> GameState
initialState gridSize = GameState
  { board = replicate gridSize (replicate gridSize 0)
  , score = 0
  , gen   = mkStdGen 0
  }

-- Function to render the game
render game = pictures
  [ drawBoard (board game)
  , drawRestartButton
  ]

-- Function to draw the game board
drawBoard :: [[Int]] -> Picture
drawBoard board = pictures $ concat
  [ [ drawTile x y val | (x, val) <- zip [0..] row] | (y, row) <- zip [0..] board]
	
-- Function to draw a single tile
drawTile :: Int -> Int -> Int -> Picture
drawTile x y val =
  translate (fromIntegral x * 100 - 150) (fromIntegral y * 100 - 150) $
    pictures
      [ color (tileColor val) $ rectangleSolid 90 90
      , translate (-20) (-20) $ scale 0.2 0.2 $ color (tileTextColor val) $ text (show val)
      ]

-- Function to determine the color of a tile based on its value
tileColor :: Int -> Color
tileColor val = case val of
  0    -> greyN 0.9
  2    -> light (light (light blue))
  4    -> light (light blue)
  8    -> light blue
  16   -> light (light (light green))
  32   -> light (light green)
  64   -> light green
  128  -> light (light (light red))
  256  -> light (light red)
  512  -> light red
  1024 -> light (light (light violet))
  2048 -> light (light violet)
  _    -> red -- Add a wildcard pattern for all other cases, you can choose any color or handle it as needed

-- Function to determine the text color of a tile based on its value
tileTextColor :: Int -> Color
tileTextColor val
  | val >= 16 = white
  | otherwise = black

-- Function to handle keyboard events
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game =
  moveAndAddRandom moveDown game
handleInput (EventKey (SpecialKey KeyDown) Down _ _) game =
  moveAndAddRandom moveUp game
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game =
  moveAndAddRandom moveLeft game
handleInput (EventKey (SpecialKey KeyRight) Down _ _) game =
  moveAndAddRandom moveRight game
handleInput (EventKey (MouseButton LeftButton) Up _ mousePos) game =
  if pointInRect mousePos restartButton
     then initialState (length (board game))
     else game  
handleInput _ game = game

-- Function to update the game state
update :: Float -> GameState -> GameState
update _ = id

-- Helper functions for game logic
moveAndAddRandom :: ([[Int]] -> [[Int]]) -> GameState -> GameState
moveAndAddRandom moveFn game =
  let newBoard = moveFn (board game)
      (emptyCells, newGen) = findEmptyCells (gen game) (length (board game))
      (x, y) = if null emptyCells then (0, 0) else head emptyCells
      newValue = if head (randomRs (0, 1) newGen :: [Int]) == 0 then 2 else 4
      updatedBoard = updateTile x y newValue newBoard
  in GameState { board = updatedBoard, score = score game, gen = newGen }

findEmptyCells :: StdGen -> Int -> ([(Int, Int)], StdGen)
findEmptyCells gen gridSize =
  let indices = [(x, y) | x <- [0..gridSize-1], y <- [0..gridSize-1]]
      emptyCells = filter (\(x, y) -> (board (initialState gridSize) !! y !! x) == 0) indices
  in (emptyCells, snd (next gen))

updateTile :: Int -> Int -> Int -> [[Int]] -> [[Int]]
updateTile x y val board =
  take y board ++
  [take x (board !! y) ++ [val] ++ drop (x + 1) (board !! y)] ++
  drop (y + 1) board

moveLeft :: [[Int]] -> [[Int]]
moveLeft = map moveRowLeft

moveRowLeft :: [Int] -> [Int]
moveRowLeft row = mergedRow ++ replicate (length row - length mergedRow) 0
  where
    mergedRow = mergeRow $ filter (/= 0) row

mergeRow :: [Int] -> [Int]
mergeRow [] = []
mergeRow [x] = [x]
mergeRow (x1 : x2 : xs)
  | x1 == x2 = x1 * 2 : mergeRow xs
  | otherwise = x1 : mergeRow (x2 : xs)

moveRight :: [[Int]] -> [[Int]]
moveRight = map (reverse . moveRowLeft . reverse)

moveUp :: [[Int]] -> [[Int]]
moveUp = transpose . moveLeft . transpose

moveDown :: [[Int]] -> [[Int]]
moveDown = transpose . moveRight . transpose

-- Main function to run the game with user input for difficulty level
main :: IO ()
main = do
  putStrLn "Choose difficulty level (easy, medium, hard):"
  difficulty <- getLine
  let gridSize = case difficulty of
                    "easy" -> 4
                    "medium" -> 6
                    "hard" -> 8
  let windowSize = gridSize * 100
  play (InWindow "2048 Game" (windowSize, windowSize) (10, 10)) white 30 (initialState gridSize) render handleInput update

-- Define where the restart button should be and its size
restartButton :: (Float, Float, Float, Float)
restartButton = (-250, 250, 150, 75)

drawRestartButton :: Picture
drawRestartButton = uncurry translate (fstPair restartButton) $ pictures
    [ color azure $ uncurry rectangleSolid (sndPair restartButton)
    , translate (-50) (-10) $ scale 0.2 0.2 $ color black $ text "Restart"
    ]

fstPair :: (Float, Float, Float, Float) -> (Float, Float)
fstPair (x, y, _, _) = (x, y)

sndPair :: (Float, Float, Float, Float) -> (Float, Float)
sndPair (_, _, w, h) = (w, h)

pointInRect :: Point -> (Float, Float, Float, Float) -> Bool
pointInRect (x, y) (rectX, rectY, width, height) =
    x >= rectX && x <= rectX + width && y >= rectY && y <= rectY + height
