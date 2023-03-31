type Cell = (Int,Int) 
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq,Ord)

member :: (Eq a) => a -> [a] -> Bool
member x [] = False
member x (y:ys) | x==y = True
                | otherwise = member x ys
				
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

up:: MyState -> MyState
up (Null) = Null
up (S (x,y) (h:t) z c ) = if (x==0) then Null else (S (x-1,y) (h:t) "up" (S (x,y) (h:t) z c ) )

down:: MyState -> MyState
down (Null) = Null
down (S (x,y) ((m,n):t) z c ) = if (x == m) then Null else (S (x+1,y) ((m,n):t) "down" (S (x,y) ((m,n):t) z c ) )

right:: MyState -> MyState
right (Null) = Null
right (S (x,y) ((m,n):t) z c ) = if (y == n) then Null else (S (x,y+1) ((m,n):t) "right" (S (x,y) ((m,n):t) z c ) )

left:: MyState -> MyState
left (Null) = Null
left (S (x,y) (h:t) z c ) = if (y==0) then Null else (S (x,y-1) (h:t) "left" (S (x,y) (h:t) z c ) )

collect:: MyState -> MyState
collect (Null) = Null
collect (S (x,y) (h:t) z c ) = if member (x,y) (h:t) then (S (x,y) (removeItem (x,y) (h:t)) "collect" (S (x,y) (h:t) z c ) ) else Null

nextMyStates::MyState->[MyState]
nextMyStates (bahy) = removeItem Null [up bahy , down bahy , left bahy , right bahy , collect bahy] 

isGoal::MyState->Bool
isGoal (Null) = False
isGoal (S (x,y) t z c ) = if t ==[] then True else False

search::[MyState]->MyState
search [] = Null
search (h:t) = if isGoal h then h else search (t ++ (nextMyStates h))

constructSolution:: MyState ->[String]
constructSolution (S (x,y) t "" c ) = []
constructSolution (S (x,y) t z c ) = (constructSolution c) ++ [z]

solve :: Cell -> [Cell] -> [String]
solve r (h:t) = constructSolution(search [ (S r (h:t) "" Null )])