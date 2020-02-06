import System.IO
import Control.Monad
import LoadSave
import Data.Char
import Data.List
import Move
import System.Exit (exitSuccess)

-- | this is where the game begins to execute
main :: IO()
main = do 
    putStr "1: Load Game \n"
    putStr "2: Save Game \n"
    putStr "3: Make Move \n"
    putStr "4: Quit Game \n"
    putStr "Enter your choice: \n"
    xs <- getLine
    executechoice xs    

-- | Executes the initial choice 
executechoice:: String -> IO()
executechoice choice =  if choice == "Load Game"
                        then do putStrLn "loading game..." 
                                load
                        else if choice == "Save Game"
                        then putStrLn "Game is already up to date"
                        
                        else if choice == "Make Move" 
                        then putStrLn "Load the game first"

                        else if choice == "Quit Game" 
                        then do putStrLn "Quitting game..."
                                exitSuccess
                        else do putStrLn "Invalid choice \n"
                                main 


-- | asks the user for a command and executes it 
getchoice :: [[(Int, Int, Char, Char)]] -> [[(Int, Int, Char, Char)]] -> [String] -> String -> IO()
getchoice newPositions positions layout lastchoice = do
        putStr "1: Load Game \n"
        putStr "2: Save Game \n"
        putStr "3: Make Move \n"
        putStr "4: Quit Game \n"
        putStrLn "5: Undo"
        putStrLn "6: Redo"
        -- putStrLn " 7: Get Hint"
        choice <- getLine
        if choice == "Load Game"
        then do putStrLn "loading game..." 
                load
        else if choice == "Save Game"
        then do putStrLn "Saving Game..."
                save newPositions positions layout
        else if choice == "Make Move" 
        then do putStrLn " Enter your move"
                makeMove newPositions positions layout lastchoice
        else if choice == "Quit Game" 
        then do putStrLn " Quitting game..."
                quit newPositions positions layout
        else if choice == "Undo"
        then do
                if lastchoice /="Undo"
                        then do
                                putStrLn "Last move is undone " 
                                putStrLn (unlines (concat $ printBoard positions))
                                getchoice positions newPositions layout "Undo"
                        else do
                                putStrLn "Cannot Undo more moves"
                                getchoice  newPositions positions layout "Undo" 
        else if choice == "Redo"
        then do
                if lastchoice =="Undo"
                        then do
                                putStrLn "The undo is redone"
                                putStrLn (unlines (concat $ printBoard positions))
                                getchoice  positions newPositions layout "Redo"
                        else do
                                putStrLn "Dont have any moves to Redo"
                                getchoice   newPositions positions layout "Redo"
        -- else if choice == "Hint"
        -- then do
        --         putStrLn "your hint is in the form X Y value"
        --         hint newPositions positions layout
        else do putStrLn "Invalid choice \n"
                getchoice newPositions positions layout lastchoice
        

-- | loads the game by reading from the given file
load :: IO()
load = do putStrLn "Enter the file name you want to load"
          xs <- getLine
          contents <- readFile xs
          let layout= take (9) $ lines contents
              numbers= drop (9) $ lines contents    
              positions = loadGame layout numbers   
              newPositions=positions       
              sudokuMap = concat $ printBoard positions
          putStrLn (unlines sudokuMap)
          getchoice newPositions positions layout "Load"

-- | saves the game as given file name
save :: [[(Int, Int, Char, Char)]] ->[[(Int, Int, Char, Char)]] -> [String]->IO()
save newPositions positions layout = do 
        putStrLn "Enter the filename you want to save as"
        name <- getLine
        saveGame name layout newPositions
        getchoice newPositions positions layout "Save"

-- | inserts a number at the given location (x,y)
makeMove :: [[(Int, Int, Char, Char)]] -> [[(Int, Int, Char, Char)]] ->[String]->String->IO()
makeMove newPositions positions layout lastchoice = do
        putStrLn " Enter the location and value as X Y Value"
        details <-getLine
        let x = digitToInt $ (words details !! 0) !! 0
            y = digitToInt $ (words details !! 1) !! 0
            number = (words details !! 2) !! 0
            newestPositions= amendBoard x y number newPositions
        validmove x y number newestPositions newPositions positions layout lastchoice

-- | checks if the move is valid or not
validmove :: Int -> Int -> Char -> [[(Int, Int, Char, Char)]] ->[[(Int, Int, Char, Char)]] -> [[(Int, Int, Char, Char)]]-> [String] -> String->IO()
validmove x y number newestPositions newPositions positions layout lastchoice = do
        if (isValid x y number newestPositions)
                then
                        if gameOver newestPositions 
                                then do
                                        putStrLn "Congratulations, you finished the game!"
                                        exitSuccess
                                else do
                                        putStrLn (unlines (concat $ printBoard newestPositions))
                                        getchoice newestPositions newPositions layout "Move"
                else do
                        putStrLn "Move was NOT successful, check that the new entry is unique in its column, zone and row."
                        getchoice  newPositions positions layout lastchoice

-- | exits the game
quit :: [[(Int, Int, Char, Char)]]-> [[(Int, Int, Char, Char)]]->[String] ->IO()
quit newPositions positions layout= do 
        putStrLn " Do you want to save your current progress ? Enter Yes or No"
        a <- getLine
        if a=="Yes"
                then save newPositions positions layout
        else if a=="No"
                then exitSuccess
        else do
                putStrLn "Invalid entry"
                quit newPositions positions layout



-- hint newPositions positions layout = do 
--         let vaca=(head (vacantspaces newPositions))
--             hi= gethint vaca newPositions
--             putStrLn(show hi)
        

-- gethint :: (Int, Int, Char, Char) -> [[(Int, Int, Char, Char)]] -> [IO()]
-- gethint (a,b,c,d) newPositions = [putStrLn (show (a,b,x) | x <- ['1','2','3','4','5','6','7','8','9'] , isValid a b x newPositions==True