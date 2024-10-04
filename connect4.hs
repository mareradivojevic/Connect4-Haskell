{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import RoseTree (Rose(..))
import Data.List

type Rows = Int
type Cols = Int

data Field = C | Z | Empty deriving (Eq)
instance Show Field where
    show :: Field -> String
    show C = "C"
    show Z = "Z"
    show Empty = " "

newtype Column = Column [Field] deriving (Show)
data Board = Board Rows Cols [Column]

generateEmptyFieldList :: Int -> [Field]
generateEmptyFieldList 0 = []
generateEmptyFieldList n = Empty : generateEmptyFieldList (n-1)

generateColumns :: Int -> [Field] -> [Column]
generateColumns 0 _ = []
generateColumns n fields = Column fields : generateColumns (n-1) fields

-- kreiranje prazne table
createEmptyBoard :: Rows -> Cols -> Board
createEmptyBoard r c = Board r c (generateColumns c (generateEmptyFieldList r))

fieldAtColumnIndex :: Int -> Column -> Field
fieldAtColumnIndex _ (Column []) = error "field index out of bounds"
fieldAtColumnIndex 0 (Column (x:xs)) = x
fieldAtColumnIndex n (Column (x:xs)) = fieldAtColumnIndex (n-1) (Column xs)

nthRow :: Board -> Int -> [Field]
nthRow (Board r c columns) n = map (fieldAtColumnIndex n) columns

rowsList :: Board -> [[Field]]
rowsList (Board r c columns) = map (nthRow (Board r c columns)) [0..r-1]

printRow :: [Field] -> String
printRow fields = foldl (\acc field -> acc ++ show field ++ "|") "|" fields ++ "\n"

boardToString :: Board -> String
boardToString board = "\n" ++ foldl (\acc fields -> acc ++ printRow fields) "" (rowsList board)

-- stampanje table
printBoard :: Board -> IO ()
printBoard board = putStrLn $ boardToString board

data Player = Crveni | Zuti deriving (Eq, Show)
data BoardState = BoardState Board Player
instance Show BoardState where
    show (BoardState board player) = boardToString board ++ "Na potezu: " ++ show player ++ "\n"
newtype Move = Move (Player, Int) deriving (Eq, Show)

columnAvailable :: Column -> Bool
columnAvailable (Column []) = error "column is empty"
columnAvailable (Column (x:xs)) = x == Empty

columnFull :: Column -> Bool
columnFull column = not (columnAvailable column)

columnAtBoardIndex :: Int -> Board -> Column
columnAtBoardIndex _ (Board r c []) = error "column index out of bounds"
columnAtBoardIndex 0 (Board r c (x:xs)) = x
columnAtBoardIndex n (Board r c (x:xs)) = columnAtBoardIndex (n-1) (Board r c xs)

columnsWithIndices :: Board -> [(Int, Column)]
columnsWithIndices (Board r c columns) = map (\ind -> (ind, columnAtBoardIndex ind (Board r c columns))) [0..c-1]

-- validni potezi
validMoves :: BoardState -> [Move]
validMoves (BoardState board player) = foldl (\acc (ind, col) -> if columnAvailable col then acc ++ [Move (player, ind)] else acc) [] (columnsWithIndices board)

lastEmptyIndex :: [Field] -> Int
lastEmptyIndex [] = -1
lastEmptyIndex (x:xs)
    | x == Empty = 1 + lastEmptyIndex xs
    | otherwise = -1

addField :: Int -> Field -> [Field] -> [Field]
addField ind field (x:xs)
    | ind == 0 = field:xs
    | otherwise = x : addField (ind-1) field xs

applyMove' :: Board -> Int -> Field -> Board
applyMove' (Board r c columns) index field =
    Board r c (map (\(ind, Column fields) -> if index == ind then Column (addField (lastEmptyIndex fields) field fields) else Column fields) (columnsWithIndices (Board r c columns)))

-- odigravanje poteza
applyMove :: BoardState -> Move -> BoardState
applyMove (BoardState (Board r c columns) currentPlayer) (Move (player, columnIndex))
    | player /= currentPlayer = error ("player " ++ show currentPlayer ++ " is on the move")
    | Move (player, columnIndex) `notElem` validMoves (BoardState (Board r c columns) currentPlayer) = error "invalid move"
    | otherwise = if player == Crveni
        then BoardState (applyMove' (Board r c columns) columnIndex C) Zuti
        else BoardState (applyMove' (Board r c columns) columnIndex Z) Crveni

printBoardState :: BoardState -> IO()
printBoardState (BoardState board player) = putStrLn $ boardToString board ++ "Na potezu: " ++ show player

-- Napisati funkciju koja proverava da li je mreža u završnom stanju, to znači da je ili jedan od igrača
-- povezao četiri polja ili da je mreža popunjena, a da niko nije pobedio.

type Row = Int
type Col = Int
type Index = (Row, Col)

fieldAtIndex :: Index -> Board -> Field
fieldAtIndex (row, col) board = fieldAtColumnIndex row (columnAtBoardIndex col board)

fieldIndices :: Board -> [Index]
fieldIndices (Board r c columns) = concatMap (\row -> map (\col -> (row, col)) [0..c-1]) [0..r-1]
-- fieldIndices (Board r c columns) = concat (map (\row -> map (\col -> (row, col)) [0..c-1]) [0..r-1])

canConnect :: Index -> Board -> String -> Bool
canConnect (row, col) (Board rows cols columns) direction
    | direction == "right" = r - row >= 3
    | direction == "left" = row >= 3
    | direction == "top" = col >= 3
    | direction == "down" = c - col >= 3
    | direction == "top-right" = (r - row >= 3) && (col >= 3)
    | direction == "down-right" = (r - row >= 3) && (c - col >= 3)
    | direction == "top-left" = (row >= 3) && (col >= 3)
    | direction == "down-left" = (row >= 3) && (c - col >= 3)
    | otherwise = error "invalid direction"
    where r = rows - 1
          c = cols - 1

isConnected :: Index -> Board -> String -> Bool
isConnected (row, col) (Board r c cols) direction
    | direction == "right" = field /= Empty && field == fieldAtIndex (row + 1, col) board && field == fieldAtIndex (row + 2, col) board && field == fieldAtIndex (row + 3, col) board
    | direction == "left"  = field /= Empty && field == fieldAtIndex (row - 1, col) board && field == fieldAtIndex (row - 2, col) board && field == fieldAtIndex (row - 3, col) board
    | direction == "top"   = field /= Empty && field == fieldAtIndex (row, col - 1) board && field == fieldAtIndex (row, col - 2) board && field == fieldAtIndex (row, col - 3) board
    | direction == "down"  = field /= Empty && field == fieldAtIndex (row, col + 1) board && field == fieldAtIndex (row, col + 2) board && field == fieldAtIndex (row, col + 3) board
    | direction == "top-right"  = field /= Empty && field == fieldAtIndex (row+1,col-1) board && field == fieldAtIndex (row+2,col-2) board && field == fieldAtIndex (row+3,col-3) board
    | direction == "down-right" = field /= Empty && field == fieldAtIndex (row+1,col+1) board && field == fieldAtIndex (row+2,col+2) board && field == fieldAtIndex (row+3,col+3) board
    | direction == "top-left"   = field /= Empty && field == fieldAtIndex (row-1,col-1) board && field == fieldAtIndex (row-2,col-2) board && field == fieldAtIndex (row-3,col-3) board
    | direction == "down-left"  = field /= Empty && field == fieldAtIndex (row-1,col+1) board && field == fieldAtIndex (row-2,col+2) board && field == fieldAtIndex (row-3,col+3) board
    | otherwise = error "invalid direction"
    where field = fieldAtIndex (row, col) (Board r c cols)
          board = Board r c cols

fullBoard :: Board -> Bool
fullBoard board = foldl (\acc ind -> (fieldAtIndex ind board /= Empty) && acc) True (fieldIndices board)

endState' :: Board -> Bool
endState' board = fullBoard board || foldl (\acc ind-> (if canConnect ind board "right" then acc || isConnected ind board "right" else acc) ||
                                            (if canConnect ind board "left" then acc || isConnected ind board "left" else acc) ||
                                            (if canConnect ind board "top" then acc || isConnected ind board "top" else acc) ||
                                            (if canConnect ind board "down" then acc || isConnected ind board "down" else acc) ||
                                            (if canConnect ind board "top-right" then acc || isConnected ind board "top-right" else acc) ||
                                            (if canConnect ind board "down-right" then acc || isConnected ind board "down-right" else acc) ||
                                            (if canConnect ind board "top-left" then acc || isConnected ind board "top-left" else acc) ||
                                            (if canConnect ind board "down-left" then acc || isConnected ind board "down-left" else acc)
                                            ) False (fieldIndices board)

-- zavrsno stanje
endState :: BoardState -> Bool
endState (BoardState board player) = endState' board

-- Korišćenje strukture podataka Rose (tačka 1) napraviti funkciju koja za proizvoljno početno stanje mreže
-- kreira stablo igre sa nekim zadatim brojem poteza. U prvom nivou stabla treba da budu sve varijante sa
-- jednim odigranim potezom, u drugom sve sa dva odigrana poteza, itd. Voditi računa da završna stanja ne
-- treba da imaju decu. Za proveru sadržaja stabla koristiti funkcije nad tipom Rose napisane u okviru tačke
-- 1.

--validMoves + applyMove za svaki
applyValidMoves :: BoardState -> [BoardState]
applyValidMoves boardState = map (applyMove boardState) (validMoves boardState)

gameTree :: BoardState -> Int -> Rose BoardState
gameTree boardState 0 = Node boardState []
gameTree boardState d
    | endState boardState = Node boardState []
    | otherwise = Node boardState (map (\x -> gameTree x (d-1)) (applyValidMoves boardState))


-- GAME STATE MONAD

data GameState = Valid BoardState | Invalid String
instance Show GameState where
    show (Valid boardState) = show boardState
    show (Invalid errMsg) = errMsg

newtype GameStateOp a = GameStateOp {runState :: GameState -> (a, GameState)}

instance Functor GameStateOp where
    fmap f (GameStateOp rs) = GameStateOp $ \gs -> let (a, newGs) = rs gs
                                                   in (f a, newGs)

instance Applicative GameStateOp where
    pure x = GameStateOp $ \gs -> (x, gs)
    GameStateOp rs1 <*> GameStateOp rs2 = GameStateOp $ \gs ->
        let (f, newGs1) = rs1 gs
            (a, newGs2) = rs2 newGs1
        in  (f a, newGs2)

    
instance Monad GameStateOp where
    return = pure
    GameStateOp rs >>= f = GameStateOp $ \gs ->
        let (a, newGs) = rs gs
            (GameStateOp newRs) = f a
        in  newRs newGs

-- runState (applyGameMove mv) gameState
applyGameMove :: Int -> GameStateOp ()
applyGameMove mv = GameStateOp $ \gs ->
    case gs of
        (Valid (BoardState (Board r c columns) player))
            | fullBoard (Board r c columns) -> ((), Invalid "Adding token to full board")
            | endState (BoardState (Board r c columns) player) -> ((), Invalid "Adding token when game is over")
            | mv < 0 || mv >= c -> ((), Invalid "Column index is out of bounds") 
            | columnFull (columnAtBoardIndex mv (Board r c columns)) -> ((), Invalid "Adding token to full column")
            | otherwise -> ((), Valid (applyMove (BoardState (Board r c columns) player) (Move(player, mv))))
        (Invalid errMsg) -> ((), Invalid errMsg)

applyGameMoves :: GameStateOp ()
applyGameMoves = do
    applyGameMove 0
    applyGameMove 0
    applyGameMove 0
    applyGameMove 0
    applyGameMove 0
    applyGameMove 0

applyGameMovesFromList :: [Int] -> GameStateOp ()
applyGameMovesFromList [] = (GameStateOp $ \gs -> ((), gs))
applyGameMovesFromList (x:xs) = do
    applyGameMove x
    applyGameMovesFromList xs

-- PARSER
charToField :: Char -> Field
charToField c =
    case c of
        'C' -> C
        'Z' -> Z
        ' ' -> Empty

filterRow :: String -> String
filterRow [] = []
filterRow (x:xs) =
    case x of
        '|' -> filterRow xs
        _ -> x : filterRow xs

-- second argument need to be an empty string
splitOnLn :: String -> String -> [String]
splitOnLn [] _ = []
splitOnLn (x:xs) el
    | x == '\n' = el : splitOnLn xs ""
    | otherwise = splitOnLn xs (el ++ [x])

parseRow :: String -> [Field]
parseRow row = map charToField (filterRow row)

parseRows :: String -> [[Field]]
parseRows rows = map parseRow (splitOnLn rows "")

parseColumn :: String -> Column
parseColumn col = Column (map charToField col)

rowsToCols :: String -> [Column]
rowsToCols rows = 
    let rowStrings = map filterRow (splitOnLn rows "")
        transposedStrings = transpose rowStrings
    in  map parseColumn transposedStrings

-- input form:
-- |C| | | |\n
-- |C|Z| | |\n
-- |Z|C|Z| |\n
-- |C|Z|C|Z|\n
parseBoard :: String -> Board
parseBoard rows =
    let columns = rowsToCols rows
        (Column fields) = head columns
        totalRows = length fields
        totalCols = length columns
    in Board totalRows totalCols columns