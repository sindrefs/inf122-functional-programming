import Data.List.Split
import Data.List
import Control.Concurrent
----IMPORTS ^

--Sindre Sørensen
--glider.txt wil show a sidewise glider


--The main "thread"
--Parses and executes commands
main = do parse [] 0 (2, 3) (3, 3)
parse board size survive alive = do line <- getLine
                                    case line of ('c':' ':xs) -> do let newSize = read xs :: Int
                                                                    printBoard [] newSize
                                                                    parse [] newSize survive alive
                                                 ('n':' ':xs) -> do let ints = map (read :: String -> Int) (splitOn " " xs)
                                                                        x1 = head ints
                                                                        x2 = ints !! 1
                                                                    printBoard ((x1, x2):board) size
                                                                    parse ((x1,x2):board) size survive alive
                                                 ('d':' ':xs) -> do let ints = map (read :: String -> Int) (splitOn " " xs)
                                                                        x1 = head ints
                                                                        x2 = ints !! 1
                                                                    printBoard (removeElem (x1, x2) board) size
                                                                    parse (removeElem (x1, x2) board) size survive alive
                                                 ('s':' ':xs) -> do let ints = map (read :: String -> Int) (splitOn " " xs)
                                                                        x1 = head ints
                                                                        x2 = ints !! 1
                                                                    parse board size (x1, x2) alive
                                                 ('b':' ':xs) -> do let ints = map (read :: String -> Int) (splitOn " " xs)
                                                                        x1 = head ints
                                                                        x2 = ints !! 1
                                                                    parse board size survive (x1, x2)
                                                 ('?':xs) -> do putStr $ "Required to survive " ++ show (fst survive) ++ " "++ show (snd survive)
                                                                        ++ "\nRequired to becomne alive " ++ show (fst alive) ++" " ++ show (snd survive)
                                                                parse board size survive alive
                                                 ('l':' ':xs) -> do let gens = read xs :: Int
                                                                    newwb <- live board size gens survive alive
                                                                    parse newwb size survive alive
                                                 ('w':' ':xs) -> do writeFile xs $ "s " ++ show (fst survive) ++ " " ++ show (snd survive)
                                                                                   ++ " b " ++ show (fst alive) ++ " " ++ show (snd alive)
                                                                                   ++ " " ++ show size ++ " " ++ boardToString board
                                                                    parse board size survive alive
                                                 ('r':' ':xs) -> do innhold <- readFile xs
                                                                    let rules = map (read :: String -> Int) $ take 7 (splitOn " " innhold)
                                                                        cords = map (read :: String -> Int) $  drop 7 (splitOn " " innhold)
                                                                        s = (rules !! 1, rules !! 2)
                                                                        a = (rules !! 4, rules !! 5)
                                                                        si = (rules !! 6)
                                                                        b = toTuple cords
                                                                    printBoard b si
                                                                    parse b si s a
                                                 ('C':'R':xs) -> do live board size 1 survive alive
                                                                    parse (nextStep board size survive alive) size survive alive
                                                 "q" -> return ()
                                                 _ -> parse board size survive alive
--Types
type Pos = (Int, Int)
type Board = [Pos]

--Converts list of ints to list of tuples
toTuple ::[Int] -> [(Int, Int)]
toTuple [] = []
toTuple (x1:x2:xs) = (x1, x2) : toTuple xs

--Converts board to string representation, where coordinates is seperated by whitespaces
boardToString :: Board -> String
boardToString board = do let list = map tupleToString board
                         intercalate " " list

--Converts tuple to string, seperated by whitespace
tupleToString :: (Int, Int) -> String
tupleToString tuple = (show (fst tuple)) ++ " " ++ (show (snd tuple))

--Loops through each step/generation to desired generation
live :: Board -> Int -> Int -> (Int, Int) -> (Int, Int) -> IO Board
live board size 0 survive alive = if board == (nextStep board size survive alive)
                                  then do putStr "Stable config reached\n"
                                          return board
                                  else return board
live board size gens survive alive = if board == (nextStep board size survive alive)
                                     then do putStr "Stable config reached\n"
                                             return board
                                     else do printBoard (nextStep board size survive alive) size
                                             threadDelay (100*1000)
                                             live (nextStep board size survive alive) size (gens-1) survive alive
--Administrates the printing of board
printBoard board size = do
                         clear
                         printTop size
                         mapM_ (\y -> writeRow y size board) [1..size]
                         putStr "\n"

--Removes an element from list
removeElem :: Eq a => a -> [a] -> [a]
removeElem el list = filter (\e -> e/=el) list

--Prints each row (expect upper number row)
writeRow :: Int -> Int -> Board -> IO()
writeRow y size board = do writeAtPos (if y > 9 then (0, y+1) else (2, y+1)) (show y)
                           sequence_  [writeAtPos (x+x+x+1, y+1) $ pointType board x y | x <- [1..size]]



--Prints upper row (1..n)
printTop :: Int -> IO ()
printTop int =  writeAtPos (2,1) $ concat ["" ++ extraSpace n ++ (show n) | n <- [1..int]]

--Checks if a given coordinate should be printet as living or dead, according to it's presens in board
pointType :: Board -> Int -> Int -> String
pointType board x y = if elem (x, y) board then "X" else "."

--Adds space to digist < 10, så that all digits are allocated same space
extraSpace :: Int -> String
extraSpace int = if int > 9 then " " else "  "

--Removes pos/cell from board
del :: Pos -> Board -> Board
del pos board = filter (/= pos) board

--Adds a pos/cell to board
add :: Pos -> Board -> Board
add pos board = pos:board

--Clears the screen (terminal)
clear :: IO()
clear = putStr "\ESC[2J"

--Prints a string to a position on screen (terminal)
writeAtPos :: Pos -> String -> IO()
writeAtPos p xs = do goTo p
                     putStr xs

--Finds a given position on screen
goTo :: Pos -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

--Finds next step in simulation according to rules
nextStep :: Board -> Int -> (Int, Int) -> (Int, Int) -> Board
nextStep board size survive alive  = survivors board size survive ++ births board size alive

--Finds surviving cells
survivors :: Board -> Int -> (Int, Int) -> [Pos]
survivors board size survive = [pos | pos <- board, elem (liveneighbs board size pos) [fst survive..snd survive]]

--Finds living neighbours for a giving cel/pos
liveneighbs :: Board -> Int -> Pos -> Int
liveneighbs board size pos = let allNeighbs = neighbs pos
                             in length $ [neigh | neigh <- allNeighbs, elem neigh board ]

--Determinates if a cell is present in board, in other words if it's alive or not
cellAlive :: Board -> Pos -> Bool
cellAlive board pos = not (elem pos board)

--Finds neighbours for a given cell/pos
neighbs :: Pos -> [Pos]
neighbs (x, y) = [(x-1, y-1), (x, y-1),
                  (x+1, y-1), (x-1, y),
                  (x+1, y), (x-1, y+1),
                  (x, y+1), (x+1, y+1)]

--Determinates what cells will be born for next step
births :: Board -> Int -> (Int, Int) -> [Pos]
births board size alive = [(x, y) | x <- [1..size],
                              y <- [1..size],
                              cellAlive board (x, y),
                              liveneighbs board size (x, y) >= fst alive,
                              liveneighbs board size (x, y) <= snd alive]
