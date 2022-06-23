type Cell = (Int,Int)

distance :: Cell -> Cell -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

nearestCell :: Cell -> [Cell] -> Cell
nearestCell (x, y) [] = error "no cells"
nearestCell (x, y) [h] = h
nearestCell (x, y) (h : t)
    | distance (x, y) h < distance (x, y) (nearestCell (x, y) t) = h
    | otherwise = nearestCell (x, y) t

moveToX :: Cell -> Cell -> [String]
moveToX (startX, startY) (mineX, mineY)
    | startX == mineX = []
    | startX < mineX = "down" : moveToX (startX + 1, startY) (mineX, mineY)
    | otherwise = "up" : moveToX (startX - 1, startY) (mineX, mineY)

moveToY :: Cell -> Cell -> [String]
moveToY (startX, startY) (mineX, mineY)
    | startY == mineY = []
    | startY < mineY = "right" : moveToY (startX, startY + 1) (mineX, mineY)
    | otherwise = "left" : moveToY (startX, startY - 1) (mineX, mineY)

solve :: Cell -> [Cell] -> [String]
solve start [] = []
solve start listOfCells =
    moveToX start (nearestCell start listOfCells) ++ 
    moveToY start (nearestCell start listOfCells) ++ 
    ["collect"] ++
    solve (nearestCell start listOfCells) (delete (nearestCell start listOfCells) listOfCells) 
    

delete :: Eq t => t -> [t] -> [t]
delete _ [] = []
delete x (y:ys)
    | x == y = ys
    | otherwise = y : delete x ys