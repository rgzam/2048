import System.Random

type Board = [[Int]]

boardSize :: Int
boardSize = 4

initialBoard :: IO Board
initialBoard = do
    firstTile <- getRandomTile
    secondTile <- getRandomTile
    return $ placeTile firstTile $ placeTile secondTile emptyBoard

printBoard :: Board -> IO ()
printBoard = mapM_ printRow
  where
    printRow = putStrLn . unwords . map show

getRandomTile :: IO Int
getRandomTile = do
    rand <- randomRIO (0, 9) :: IO Int
    return $ if rand < 9 then 2 else 4

placeTile :: Int -> Board -> Board
placeTile tile board =
    let emptyPositions = [(i, j) | i <- [0..boardSize-1], j <- [0..boardSize-1], board !! i !! j == 0]
        (i, j) = emptyPositions !! 0
    in replaceInList i (replaceInList j tile (board !! i)) board

replaceInList :: Int -> a -> [a] -> [a]
replaceInList n newVal list = take n list ++ [newVal] ++ drop (n + 1) list

emptyBoard :: Board
emptyBoard = replicate boardSize (replicate boardSize 0)

printHelp :: IO ()
printHelp = do
    putStrLn "Welcome to 2048 in Haskell!"
    putStrLn "Use WASD to make moves:"
    putStrLn "  W - Move Up"
    putStrLn "  A - Move Left"
    putStrLn "  S - Move Down"
    putStrLn "  D - Move Right"

main :: IO ()
main = do
    printHelp
    board <- initialBoard
    gameLoop board

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
    if newBoard /= board
        then do
            tile <- getRandomTile
            let boardWithNewTile = placeTile tile newBoard
            gameLoop boardWithNewTile
        else do
            putStrLn "Invalid move! Try again."
            gameLoop board

moveUp, moveLeft, moveDown, moveRight :: Board -> Board
moveUp = transpose . moveLeft . transpose
moveLeft = map mergeRow
moveDown = transpose . moveRight . transpose
moveRight = map (reverse . mergeRow . reverse)

mergeRow :: [Int] -> [Int]
mergeRow row = merged ++ padding
  where
    merged = combine $ filter (/= 0) row
    padding = replicate (length row - length merged) 0

combine :: [Int] -> [Int]
combine [] = []
combine [x] = [x]
combine (x:y:xs)
    | x == y = x * 2 : combine xs
    | otherwise = x : combine (y : xs)

transpose :: Board -> Board
transpose ([]:_) = []
transpose m = (map head m) : transpose (map tail m)
