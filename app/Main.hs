import System.Random

type Board = [[Int]]
boardSize :: Int
boardSize = 4

--creates an IO action that generates the initial game board with two rand tiles
--emptyboard is a 4x4 matrix filled with zeros
initialBoard :: IO Board
initialBoard = do
    firstTile <- getRandomTile
    secondTile <- getRandomTile
    return $ placeTile firstTile $ placeTile secondTile emptyBoard

--prints the gameboard row by row
printBoard :: Board -> IO ()
printBoard = mapM_ printRow
  where
    printRow = putStrLn . unwords . map show


--function generates a random number between 0 and 9, and based on whether it's less than 9 
--or not, it returns either 2 or 4. This simulates the behavior of the 2048
--game where the probability of getting a 2 is higher than getting a 4.
--Generates a random tile (2 or 4) with a higher probability for 2
getRandomTile :: IO Int 
getRandomTile = do
    rand <- randomRIO (0s, 9) :: IO Int 
    return $ if rand < 9 then 2 else 4 

--places a given tile value at a randomly chosen empty position on the board 
--and returns the updated board. This function is typically used to add a new tile
--to the board after a valid move in the 2048 game.
--places a tile on an empty position on the baord
placeTile :: Int -> Board -> Board
placeTile tile board =
    let emptyPositions = [(i, j) | i <- [0..boardSize-1], j <- [0..boardSize-1], board !! i !! j == 0] 
        (i, j) = emptyPositions !! 0 
--It effectively chooses a random empty position, as the list of empty positions is generated randomly.
    in replaceInList i (replaceInList j tile (board !! i)) board 

--a utility function used to replace an element at a specified index in a list with a new value.
replaceInList :: Int -> a -> [a] -> [a]
replaceInList n newVal list = take n list ++ [newVal] ++ drop (n + 1) list
--replaceInList 2 9 [1, 2, 3, 4, 5] => The result would be [1, 9, 3, 4, 5]

--represents an empty 4x4 game board
emptyBoard :: Board
emptyBoard = replicate boardSize (replicate boardSize 0)

--this prints our welcome message
printHelp :: IO ()
printHelp = do
    putStrLn "Welcome to 2048 in Haskell!"
    putStrLn "Use WASD to make moves:"
    putStrLn "  W - Move Up"
    putStrLn "  A - Move Left"
    putStrLn "  S - Move Down"
    putStrLn "  D - Move Right"

--prints the help message, generates the initial board, and starts the game loop
main :: IO ()
main = do
    printHelp
    board <- initialBoard
    gameLoop board

--The main game loop that prints the board, prompts the user for a move, and updates the board accordingly.
--If a valid move is made, a new tile is placed, and the game loop continues. Otherwise, an error message is displayed.
gameLoop :: Board -> IO ()
gameLoop board = do
    printBoard board
    putStrLn "Enter a move (WASD): "
    move <- getLine 
    let newBoard = case move of
            "w" -> moveUp board
            "a" -> moveLeft board
            "s" -> moveDown board
            "d" -> moveRight board
            _   -> board
    if newBoard /= board --Checks if the new board state is different from the current board. 
        then do
            tile <- getRandomTile
            let boardWithNewTile = placeTile tile newBoard
            gameLoop boardWithNewTile
        else do
            putStrLn "Invalid move! Try again."
            gameLoop board
-- perform the respective moves by manipulating the board.
--transpose function transposes a matrix.
moveUp, moveLeft, moveDown, moveRight :: Board -> Board
moveUp = transpose . moveLeft . transpose
moveLeft = map mergeRow
moveDown = transpose . moveRight . transpose
moveRight = map (reverse . mergeRow . reverse)
--Merges and slides numbers in a row to the left after a move.
mergeRow :: [Int] -> [Int]
mergeRow row = merged ++ padding
  where
    merged = combine $ filter (/= 0) row
    padding = replicate (length row - length merged) 0
--Combines adjacent equal tiles in a row.
combine :: [Int] -> [Int]
combine [] = []
combine [x] = [x]
combine (x:y:xs)
    | x == y = x * 2 : combine xs
    | otherwise = x : combine (y : xs)

transpose :: Board -> Board
transpose ([]:_) = []--If the input board is empty (has no rows), the transposed board is also empty.
transpose m = (map head m) : transpose (map tail m)