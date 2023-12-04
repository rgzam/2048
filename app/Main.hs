import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.List (transpose)

-- Define the game state
data GameState = GameState
  { board      :: [[Int]]
  , score      :: Int
  , gen        :: StdGen
  , difficulty :: String
  , menuShown  :: Bool
  }

-- Function to set the initial grid size based on difficulty
initialGridSize :: String -> Int
initialGridSize difficulty =
  case difficulty of
    "easy"   -> 4
    "medium" -> 5
    "hard"   -> 6
    _        -> error "Invalid difficulty"

-- Initial game state with a specified grid size
initialState :: GameState
initialState = GameState
  { board      = replicate (initialGridSize "easy") (replicate (initialGridSize "easy") 0)
  , score      = 0
  , gen        = mkStdGen 0
  , difficulty = "easy"
  , menuShown  = True
  }

-- Function to render the game
render :: GameState -> Picture
render game
  | menuShown game = renderDifficultyMenu game -- Render difficulty menu
  | otherwise  = renderGame game             -- Render the game

-- Function to render the difficulty menu
renderDifficultyMenu :: GameState -> Picture
renderDifficultyMenu game = pictures
  [ drawText "Choose Difficulty:" (-150) 150
  , drawText "1. Easy" (-150) 50
  , drawText "2. Medium" (-150) 0
  , drawText "3. Hard" (-150) (-50)
  ]

-- Function to render the game with initial text for difficulty level
renderGame :: GameState -> Picture
renderGame game = pictures
  [ drawTextSmall ("Difficulty: " ++ difficulty game) (-150) 210  -- Smaller text
  , drawBoard (board game)
  ]

-- Function to draw smaller text on the screen
drawTextSmall :: String -> Float -> Float -> Picture
drawTextSmall textString x y = translate x y $ scale 0.2 0.2 $ color black $ text textString

drawBoard :: [[Int]] -> Picture
drawBoard board = pictures $ concat
    [ [ drawTile x y val | (x, val) <- zip [0..] row] | (y, row) <- zip [0..] board]

-- Function to render a single tile
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
  _    -> red -- Add a wildcard pattern for all other cases; you can choose any color or handle it as needed

-- Function to determine the text color of a tile based on its value
tileTextColor :: Int -> Color
tileTextColor val
  | val >= 16 = white
  | otherwise = black

-- Function to draw text on the screen
drawText :: String -> Float -> Float -> Picture
drawText textString x y = translate x y $ scale 0.3 0.3 $ color black $ text textString

handleInput :: Event -> GameState -> GameState
handleInput e game =
  case e of
    EventKey (Char '1') Down _ _ -> startGame "easy" game
    EventKey (Char '2') Down _ _ -> startGame "medium" game
    EventKey (Char '3') Down _ _ -> startGame "hard" game
    EventKey (SpecialKey KeyUp) Down _ _ -> moveAndAddRandom moveDown game
    EventKey (SpecialKey KeyDown) Down _ _ -> moveAndAddRandom moveUp game
    EventKey (SpecialKey KeyLeft) Down _ _ -> moveAndAddRandom moveLeft game
    EventKey (SpecialKey KeyRight) Down _ _ -> moveAndAddRandom moveRight game
    EventKey (Char 'r') Down _ _ -> restartGame game  -- Handle restart option
    _ -> game

-- Function to restart the game
restartGame :: GameState -> GameState
restartGame game =
  initialState { difficulty = difficulty game, menuShown = False }

-- Function to start the game with a chosen difficulty and set the grid size
startGame :: String -> GameState -> GameState
startGame chosenDifficulty game =
  game { difficulty = chosenDifficulty, board = replicate (initialGridSize chosenDifficulty) (replicate (initialGridSize chosenDifficulty) 0), menuShown = False }

-- Function to update the game state
update :: Float -> GameState -> GameState
update _ game
  | menuShown game = game  -- If the menu is shown, do nothing
  | otherwise      = game  -- If the menu is not shown, do the update (or replace 'game' with your logic)

-- Helper functions for game logic
moveAndAddRandom :: ([[Int]] -> [[Int]]) -> GameState -> GameState
moveAndAddRandom moveFn game =
  let newBoard = moveFn (board game)
      (emptyCells, newGen) = findEmptyCells game (gen game) (length (board game))
      (x, y) = if null emptyCells then (0, 0) else head emptyCells
      newValue = if head (randomRs (0, 1) newGen :: [Int]) == 0 then 2 else 4
      updatedBoard = updateTile x y newValue newBoard
  in game { board = updatedBoard, gen = newGen, menuShown = False }  -- Update menuShown as needed

findEmptyCells :: GameState -> StdGen -> Int -> ([(Int, Int)], StdGen)
findEmptyCells game gen gridSize =
  let indices = [(x, y) | x <- [0..gridSize-1], y <- [0..gridSize-1]]
      emptyCells = filter (\(x, y) -> (board game !! y !! x) == 0) indices
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
moveUp = transpose . map moveRowUp . transpose

moveDown :: [[Int]] -> [[Int]]
moveDown = transpose . map moveRowDown . transpose

moveRowUp :: [Int] -> [Int]
moveRowUp row = mergedRow ++ replicate (length row - length mergedRow) 0
  where
    mergedRow = mergeRow $ filter (/= 0) row

moveRowDown :: [Int] -> [Int]
moveRowDown row = replicate (length row - length mergedRow) 0 ++ mergedRow
  where
    mergedRow = mergeRow $ filter (/= 0) row

main :: IO ()
main = do
  putStrLn "Welcome to 2048 Game!"
  let windowSize = 400
  play (InWindow "2048 Game" (windowSize, windowSize) (10, 10)) white 30 initialState render handleInput update
