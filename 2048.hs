module B2048 where
import System.Random


-- globals
width = 4
height = 4
type Board = [[Int]]


remove0 :: [Int] -> [Int]
remove0 [] = []
remove0 (0:xs) = remove0 xs
remove0 (x:xs) = x : remove0 xs


move :: [Int] -> Int -> [Int]
move [] n = replicate (abs n) 0
move (x:xs) n = if length (x:xs) < abs n then
    if n < 0
        then (replicate (abs (n)-length (x:xs)) 0) ++ (x:xs)
        else (x:xs) ++ (replicate (abs (n)-length (x:xs)) 0)
    else x:xs

-- move to start e move to end
moveS :: [Int] -> [Int]
moveS l = move (remove0 l) (length l)
moveE :: [Int] -> [Int]
moveE l = move (remove0 l) (-(length l))

-- merge basico
merge :: [Int] -> [Int]
merge [] = []
merge [x] = [x]
merge ( x : y : xs ) =
    if x == y && x /= 0
        then 2 * x : 0 : ( merge xs )
        else x : ( merge (y:xs) )

-- merge to start e merge to end
mergeS :: [Int] -> [Int]
mergeS l = (moveS.merge.moveS) l
mergeE :: [Int] -> [Int]
mergeE l = (moveE.reverse.merge.reverse.moveE) l

transpose :: Board -> Board
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

rotateL :: Board -> Board
rotateL = reverse . transpose
rotateR :: Board -> Board
rotateR = transpose . reverse

left :: Board -> Board
left board = map mergeS board

right :: Board -> Board
right board = map mergeE board

up :: Board -> Board
up board = rotateL( map mergeE (rotateR board) )

down :: Board -> Board
down board = rotateL( map mergeS (rotateR board) )



getEmpty :: Board -> [(Int, Int)]
getEmpty board =
    [(x, y) |   y <- [0..(length board)-1],
                x <- [0..(length (board !! 0))-1],
                board !! y !! x == 0]

getRandom :: Board -> IO (Int, Int)
getRandom board = do
    let e = getEmpty board
    if null e
        then error "erro"
        else do
            r <- randomRIO (0, (length e - 1))
            return (e !! r)

randInt :: Int -> Int -> IO Int
randInt l u = randomRIO (l, u)

random24 :: Int -> IO Int
random24 x = do
    r <- (randInt 0 100)
    if r < x
        then return 4
        else return 2



createBoard :: Int -> Int -> Board
createBoard x y = replicate y (replicate x 0)

setRow :: [Int] -> Int -> Int -> [Int]
setRow l i v = 
    take i l ++ 
    [v] ++ 
    drop (i+1) l

setBoard :: Board -> Int -> Int -> Int -> Board
setBoard board x y v = 
    take y board ++ 
    [setRow (board !! y) x v] ++ 
    drop (y + 1) board



addRandom :: Board -> IO Board
addRandom board = do
    rp <- getRandom board
    num <- random24 20
    return (setBoard board (fst rp) (snd rp) num)

start :: IO Board
start = do
    let b = createBoard width height
    b0 <- addRandom b
    b1 <- addRandom b0

    return b1


printGrid :: Board -> IO ()
printGrid grid = putStrLn $ unlines $ map showRow grid
    where
        showRow :: [Int] -> String
        showRow row = unwords $ map show row


getQtNum :: Board -> Int -> Int
getQtNum board n = length (filter (== n) (concat board))

else0 :: Int -> Int
else0 x = if x < 0 then 0 else x

getPoints :: Board -> Board -> Int
getPoints b0 b1 = 
    sum $ map 
        (\n -> else0 (((getQtNum b0 n) - (getQtNum b1 n)) * n))
        (take 50 [2^x | x <- [1..]])
    


loop :: Board -> Int -> IO()
loop board points = do
    putStrLn (show points)
    printGrid board
    input <- getChar
    

    if input == 'u' then do
        putStrLn "up"
        let m = up board
        if board == m then do
            putStrLn "Move didn't make changes."
            loop board points
        else do
            let p = getPoints board m
            b0 <- addRandom m
            loop b0 (points + p)
    else if input == 'r' then do
        putStrLn "right"
        let m = right board
        if board == m then do
            putStrLn "Move didn't make changes."
            loop board points
        else do
            let p = getPoints board m
            b0 <- addRandom m
            loop b0 (points + p)
    else if input == 'd' then do
        putStrLn "down"
        let m = down board
        if board == m then do
            putStrLn "Move didn't make changes."
            loop board points
        else do
            let p = getPoints board m
            b0 <- addRandom m
            loop b0 (points + p)
    else if input == 'l' then do
        putStrLn "left"
        let m = left board
        if board == m then do
            putStrLn "Move didn't make changes."
            loop board points
        else do
            let p = getPoints board m
            b0 <- addRandom m
            loop b0 (points + p)
    else if input == 'x' then do
        putStrLn "Exiting."
        printGrid board
    else 
        loop board points



main :: IO()
main = do
    putStrLn "Controls:\nu: up\nr: right\nd: down\nl: left\nx: exit\n"
    b <- start
    loop b 0
