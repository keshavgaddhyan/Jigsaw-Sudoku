module LoadSave
( loadGame
, saveGame 
, printBoard
)where

-- | This funcion loads the game and stores each element of the bpard 
loadGame layout numbers =  [ sectorValue y row numbers | (y,row)<- zip[0..] layout] 


-- | This functions assigns sector value to each tuple
sectorValue :: Int -> String -> [String] -> [(Int, Int, Char, Char)]
sectorValue y row numbers =[(x, y, sector, actualValue x y numbers)| (x, sector) <-zip [0..] row] 
                         where
                            actualValue x y numbers = ((numbers !! y) !! x)

-- | This function saves the game to a specified file
saveGame :: String -> [String] -> [[(Int, Int, Char, Char)]] -> IO ()
saveGame filename layout positions = writeFile filename (unlines (layout ++ (valuesToRows positions)))

-- | Coverts the tuples into rows to write to a file
valuesToRows :: [[(Int, Int, Char, Char)]] -> [String]
valuesToRows positions = [convert row | row <-positions ]
                           where 
                            convert row = [value | (x, y, sector, value) <-row]

-- | Displays the jigsaw sudoku board on the screen
printBoard :: [[(Int, Int, Char, Char)]] -> [[String]]
printBoard positions = [["|" ++ (replicate (45) '-') ++ "|"]] ++ [[printSector row positions, printBarrier row positions]| row <- positions]

-- | Displays Barriers
printBarrier :: [(Int, Int, Char, Char)] -> [[(Int, Int, Char, Char)]] -> String
printBarrier row positions = "|" ++ (concat [printBarrierRow coord positions| coord <- row]) ++ "|"

-- | Displays Sectors
printSector :: [(Int, Int, Char, Char)] -> [[(Int, Int, Char, Char)]] -> String
printSector row positions =  "|" ++ (concat [printSectorRow coord positions| coord <- row]) ++ "|"

-- | Displays rows in the sector
printSectorRow :: (Int, Int, Char, Char) -> [[(Int, Int, Char, Char)]] -> String
printSectorRow (x, y, sector, value) positions
  | x == 8 = "  " ++ [value] ++ "  "
  | getSector (x + 1) y positions /= sector = "  " ++ [value] ++ " |"
  | otherwise = "  " ++ [value] ++ "  "
 
-- | Displays barrier rows
printBarrierRow :: (Int, Int, Char, Char) -> [[(Int, Int, Char, Char)]] -> String
printBarrierRow (x, y, sector, value) positions
  | y == 8 = "-----"
  | x == 8 && getSector x (y + 1) positions /= sector = "-----"
  | x == 8 && getSector x (y + 1) positions == sector  = "     "
  | getSector x (y + 1) positions /= sector = "-----"
  | getSector (x + 1) y positions /= sector = "    |"
  | otherwise = "     "

-- | Returns the sector given (x,y) and the positions variable
getSector :: Int -> Int -> [[(Int, Int, Char, Char)]] -> Char
getSector x y positions = getSec ((positions !! y) !! x)
                          where
                            getSec (x, y, sector, value) = sector