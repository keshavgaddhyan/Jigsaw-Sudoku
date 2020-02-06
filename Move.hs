module Move
( amendBoard
, isValid
, gameOver
, vacantspaces
) where

-- | This function add a new value to the sudoku board at the location given by the user
amendBoard :: Int -> Int -> Char -> [[(Int, Int, Char, Char)]] -> [[(Int, Int, Char, Char)]] 
amendBoard x y number positions = group 9 $ [returntuple x y number tuple | row <- positions, tuple <- row]
                                  where
                                    group :: Int -> [a] -> [[a]]
                                    group _ [] = []
                                    group n l = (take n l) : (group n (drop n l))

-- | Inserts the number at the required position if location matches  and then returns the tuple, otherwise returns the normal tuple
returntuple :: Int -> Int -> Char -> (Int, Int, Char, Char) -> (Int, Int, Char, Char)
returntuple x y number position = if x==getX position && y==getY position then
                                  (x, y, getSectorPos position, number)
                                  else position
-- | Returns value to sector
getSectorPos :: (Int, Int, Char, Char) -> Char
getSectorPos (x, y, zone, value) = zone

-- | returns y location
getY :: (Int, Int, Char, Char) -> Int
getY (x, y, zone, value) = y

-- | returns x location
getX :: (Int, Int, Char, Char) -> Int
getX (x, y, zone, value) = x

-- | checks if the given number is valid to be inserted at the given location
isValid :: Int -> Int -> Char -> [[(Int, Int, Char, Char)]] -> Bool
isValid x y number positions = (validSector number zone) && (validRow number row) &&  (validColumn number column) 
  where row = positions !! y
        column = [row !! x | row <- positions]
        zone = concat [sectorValues row (getSector x y positions) | row <- positions]
        
-- | filters the values in the sector
sectorValues :: [(Int, Int, Char, Char)] -> Char -> [(Int, Int, Char, Char)]
sectorValues row reqSector  = filter (\(x, y, zone, number) -> zone == reqSector) row

-- | checks if the number is valid in the given row
validRow :: Char -> [(Int, Int, Char, Char)] -> Bool
validRow number row =  (length (filter (\(x, y, zone, value) -> value == number) row)) <= 1

-- | checks if the number is valid in the given column
validColumn :: Char -> [(Int, Int, Char, Char)] -> Bool
validColumn number column =  (length (filter (\(x, y, zone, value) -> value == number) column)) <= 1

-- | checks if the number is valid in the given sector
validSector :: Char -> [(Int, Int, Char, Char)] -> Bool
validSector number zone =  (length (filter (\(x, y, zone, value) -> value == number) zone)) <= 1

-- | checks if the board is complete with all valid rows ans columns and sectors
gameOver :: [[(Int, Int, Char, Char)]] -> Bool
gameOver positions = length (vacantspaces positions) ==0

-- | retuens tuples which do not have any value inserted
vacantspaces :: [[(Int, Int, Char, Char)]] ->[(Int, Int, Char, Char)]
vacantspaces positions = [ tuples | row <- positions, tuples <- row , isempty tuples]
                          where
                            isempty :: (Int , Int ,Char, Char) -> Bool
                            isempty ( a , b , c , d)
                              | d =='.' = True
                              | otherwise = False
-- | returns the sector 
getSector :: Int -> Int -> [[(Int, Int, Char, Char)]] -> Char
getSector x y coords = getSectorPos ((coords !! y) !! x)