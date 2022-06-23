type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

up:: MyState -> MyState
up Null = Null
up (S (0,y) listOfCells previousAction previousState) = Null
up (S (x,y) listOfCells previousAction previousState) 
    = S (x - 1, y) listOfCells "up" (S (x,y) listOfCells previousAction previousState)

down:: MyState -> MyState
down Null = Null
down (S (3,y) listOfCells previousAction previousState) = Null
down (S (x,y) listOfCells previousAction previousState) 
    = S (x + 1, y) listOfCells "down" (S (x,y) listOfCells previousAction previousState)

left:: MyState -> MyState
left Null = Null
left (S (x, 0) listOfCells previousAction previousState) = Null
left (S (x, y) listOfCells previousAction previousState) 
    = S (x, y - 1) listOfCells "left" (S (x,y) listOfCells previousAction previousState)

right:: MyState -> MyState
right Null = Null
right (S (x, 3) listOfCells previousAction previousState) = Null
right (S (x, y) listOfCells previousAction previousState) 
    = S (x, y + 1) listOfCells "right" (S (x,y) listOfCells previousAction previousState)

collect:: MyState -> MyState
collect Null = Null
collect (S (x,y) listOfCells previousAction previousState) 
    | elem (x, y) listOfCells = S (x,y) (delete (x, y) listOfCells) "collect" (S (x,y) listOfCells previousAction previousState)
    | otherwise = Null

nextMyStates::MyState->[MyState]
nextMyStates Null = []
nextMyStates (S (x,y) listOfCells previousAction previousState) 
    = filter notNull 
            (map (\f -> f (S (x,y) listOfCells previousAction previousState)) 
                 [up, down, left, right, collect])

isGoal :: MyState -> Bool
isGoal Null = False
isGoal (S (x,y) [] previousAction previousState) = True
isGoal (S (x,y) listOfCells previousAction previousState) = False

search :: [MyState] -> MyState
search [] = Null
search (x:xs)
    | isGoal x = x
    | otherwise = search (xs ++ nextMyStates x)

constructSolution :: MyState -> [String]
constructSolution Null = []
constructSolution (S (x,y) listOfCells "" previousState) = []
constructSolution (S (x,y) listOfCells previousAction previousState) 
    = constructSolution previousState ++ [previousAction]

solve :: Cell -> [Cell] -> [String]
solve start listOfCells = constructSolution (search [S start listOfCells "" Null])


notNull::MyState -> Bool
notNull Null = False
notNull _ = True

delete :: Eq t => t -> [t] -> [t]
delete _ [] = []
delete x (y:ys)
    | x == y = ys
    | otherwise = y : delete x ys

