import Debug.Trace

type SudokuGrid = [[Cell]]

data Cell = Fixed {number :: Int} | NotFixed {number :: Int} deriving (Show)

grid :: [[Cell]]
grid = [[NotFixed 0,NotFixed 0,Fixed 8,NotFixed 0,NotFixed 0,Fixed 7,NotFixed 0,NotFixed 0,Fixed 6],
        [NotFixed 0,Fixed 5,NotFixed 0,NotFixed 0,Fixed 4,NotFixed 0,NotFixed 0,Fixed 7,NotFixed 0],
        [Fixed 2,NotFixed 0,NotFixed 0,Fixed 1,NotFixed 0,NotFixed 0,Fixed 8,NotFixed 0,NotFixed 0],
        [NotFixed 0,Fixed 3,NotFixed 0,NotFixed 0,Fixed 9,NotFixed 0,NotFixed 0,Fixed 1,NotFixed 0],
        [Fixed 1,NotFixed 0,Fixed 7,Fixed 2,NotFixed 0,Fixed 8,Fixed 3,NotFixed 0,Fixed 9],
        [NotFixed 0,Fixed 8,NotFixed 0,NotFixed 0,Fixed 6,NotFixed 0,NotFixed 0,Fixed 2,NotFixed 0],
        [NotFixed 0,NotFixed 0,Fixed 9,NotFixed 0,NotFixed 0,Fixed 3,NotFixed 0,NotFixed 0,Fixed 1],
        [NotFixed 0,Fixed 6,NotFixed 0,NotFixed 0,Fixed 1,NotFixed 0,NotFixed 0,Fixed 4,NotFixed 0],
        [Fixed 7,NotFixed 0,NotFixed 0,Fixed 6,NotFixed 0,NotFixed 0,Fixed 2,NotFixed 0,NotFixed 0]]

{--
grid' :: [[Cell]]
grid' = [[NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0]]
--}
grid' :: [[Cell]]
grid' = [[NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0,NotFixed 0]]

grideasy :: [[Cell]]
grideasy = [[Fixed 5,NotFixed 0,NotFixed 0,Fixed 2,NotFixed 0,NotFixed 0,Fixed 1,Fixed 4,Fixed 8],
        [NotFixed 0,Fixed 6,NotFixed 0,NotFixed 0,Fixed 9,NotFixed 0,NotFixed 0,Fixed 7, NotFixed 0],
        [Fixed 2,Fixed 7,NotFixed 0,NotFixed 0,Fixed 5,Fixed 8,NotFixed 0,NotFixed 0,NotFixed 0],
        [NotFixed 0,NotFixed 0,Fixed 7,NotFixed 0,Fixed 1,Fixed 6,Fixed 9,Fixed 5,NotFixed 0],
        [Fixed 9,Fixed 1,Fixed 5,NotFixed 0,NotFixed 0,NotFixed 0,Fixed 3,Fixed 8,NotFixed 0],
        [NotFixed 0,NotFixed 0,Fixed 2,Fixed 3,Fixed 5,Fixed 9,Fixed 4,NotFixed 0,NotFixed 0],
        [NotFixed 0,Fixed 9,NotFixed 0,Fixed 7,Fixed 2,NotFixed 0,NotFixed 0,Fixed 6,Fixed 1],
        [NotFixed 0,Fixed 2,Fixed 6,NotFixed 0,NotFixed 0,Fixed 5,Fixed 8,NotFixed 0,Fixed 9],
        [Fixed 1,NotFixed 0,Fixed 8,NotFixed 0,Fixed 9,Fixed 3,NotFixed 0,NotFixed 0,Fixed 4]]

isInVert :: Int -> Int -> SudokuGrid -> Bool
isInVert n colNumber grid = n `elem` (map number $ map (!!colNumber) grid)

isInHor :: Int -> Int -> SudokuGrid -> Bool
isInHor n lineNumber grid = n `elem` (map number $ grid!!lineNumber)

isInZone :: Int -> Int -> Int -> SudokuGrid -> Bool
isInZone n x y grid = n `elem` (foldr (++) [] $ getZone x y grid)

getZone :: Int -> Int -> SudokuGrid -> [[Int]] 
getZone x y grid 
        | x < 3 && y < 3   = returnGrid 0 0 grid
        | x < 3 && y < 6   = returnGrid 0 3 grid 
        | x < 3 && y < 9  = returnGrid 0 6 grid
        | x < 6 && y < 3   = returnGrid 3 0 grid
        | x < 6 && y < 6   = returnGrid 3 3 grid
        | x < 6 && y < 9  = returnGrid 3 6 grid
        | x < 9 && y < 3  = returnGrid 6 0 grid
        | x < 9 && y < 6  = returnGrid 6 3 grid
        | x < 9 && y < 9 = returnGrid 6 6 grid
        | otherwise = error "out of bound"
        where returnGrid x y grid = map (map number) (map (take 3 . drop y)(take 3 $ drop x grid))
                
getNumber :: Int -> Int -> SudokuGrid -> Cell
getNumber x y grid =  head $ drop y $ head $ drop x grid

replaceElementAt :: Int -> Int -> Int -> SudokuGrid -> SudokuGrid
replaceElementAt x y replacement grid = (take (x) grid) ++ [((take (y) $ grid!!x)++[NotFixed replacement]++(drop (y+1) $ grid!!(x)))] ++ (drop (x+1) grid)

getPre :: Int -> Int -> (Int, Int)
getPre 0 0 = (0, 0)
getPre x 0 = ((x-1), 8) 
getPre x y = (x, (y-1))

getNext :: Int -> Int -> (Int, Int)
getNext 0 0 = (0, 0)
getNext x 8 = ((x+1), 0) 
getNext x y = (x, (y+1))

assignNumber :: Int -> Int -> SudokuGrid -> SudokuGrid 
assignNumber x y grid  
        | isFixed $ getNumber x y grid = grid 
        | (isFixed $ getNumber x y grid) == False = let n = number $ getNumber x y grid in 
                                                    replaceElementAt x y (getNumberNotPresent x y n grid) grid 
        | otherwise = grid

isFixed :: Cell -> Bool
isFixed (Fixed _) = True 
isFixed (NotFixed _) = False 

getPreNotFixed :: Int -> Int -> SudokuGrid -> (Int, Int)
getPreNotFixed 0 0  _ = (0, 0)
getPreNotFixed x 0 s = if (isFixed $ getNumber (x-1) 8 s) then getPreNotFixed (x-1) 8 s else ((x-1),8)
getPreNotFixed x y s = if (isFixed $ getNumber x (y-1) s) then getPreNotFixed x (y-1) s else (x,(y-1))

getNumberNotPresent :: Int -> Int -> Int -> SudokuGrid -> Int
getNumberNotPresent x y 10 grid = 0
getNumberNotPresent x y number grid 
        | isInVert number y grid || isInHor number x grid || isInZone number x y grid || number == 0 = getNumberNotPresent x y (number + 1) grid
        | otherwise = number 

generateSolution :: Int -> Int -> SudokuGrid -> SudokuGrid
generateSolution 0 0 grid = generateSolution 0 1 $ assignNumber 0 0 grid
generateSolution 8 8 grid = let (x',y') = getPreNotFixed 8 8 grid in 
                            if ((getNumberNotPresent 8 8 (number $ getNumber 8 8 grid) grid) == 0 && (not $ isFixed $ getNumber 8 8 grid)) 
                            then generateSolution x' y' $ replaceElementAt 8 8 0 grid
                            else assignNumber 8 8 grid
generateSolution x y grid = let (x',y') = getPreNotFixed x y grid; (x'',y'') = getNext x y in 
                            if ((getNumberNotPresent x y (number $ getNumber x y grid) grid) == 0 && (not $ isFixed $ getNumber x y grid)) 
                            --then trace (show x ++ (show y) ++ (show x') ++ (show y') ++ show(x'') ++ (show y'')) $ generateSolution x' y' grid
                            then generateSolution x' y' $ replaceElementAt x y 0 grid
                            --else generateSolution x'' y'' $ assignNumber x y grid
                            else trace (show $ map (map number) grid) generateSolution x'' y'' $ assignNumber x y grid


{--
generateSolution :: Int -> Int -> SudokuGrid -> SudokuGrid
generateSolution 0 0 grid = generateSolution 0 1 (assignNumber 0 0 grid)
generateSolution 8 8 grid = if ((number $ getNumber 8 7 grid) == 0) then generateSolution 8 7 $ assignNumber 8 6 $ zeroFrom 8 8 grid else assignNumber 8 8 grid
generateSolution x 8 grid = let (x', y') = getPre x 8; (x'', y'') = getPre x' y' in if ((number $ getNumber x' y' grid) == 0) then generateSolution x' y' $ assignNumber x'' y'' $ zeroFrom x 8 grid else generateSolution (x+1) 0 (assignNumber x 8 grid)
generateSolution x y grid = let (x', y') = getPre x y; (x'', y'') = getPre x' y' in if ((number $ getNumber x' y' grid) == 0) then generateSolution x' y' $ assignNumber x'' y'' $ zeroFrom x y grid else generateSolution x (y+1) (assignNumber x y grid)
--}

{--
zeroFrom :: Int -> Int -> SudokuGrid -> SudokuGrid
zeroFrom 8 8 grid = if (isFixed $ getNumber 8 8 grid) then grid else replaceElementAt 8 8 0 grid 
zeroFrom x 8 grid = if (isFixed $ getNumber x 8 grid) then zeroFrom (x+1) 0 grid else zeroFrom (x+1) 0 $ replaceElementAt x 8 0 grid
zeroFrom x y grid = if (isFixed $ getNumber x y grid) then zeroFrom x (y+1) grid else zeroFrom x (y+1) $ replaceElementAt x y 0 grid
--}
