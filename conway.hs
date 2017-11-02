import Control.Concurrent
xoper = [(\x->x-1),(id),(+1)]
yoper = [(\x -> x-1),(id),(+1)]
ogrid = do {
    x <- xoper;
    y <- yoper;
    return (x,y)
}

--Hardcoded in a cool pattern. Bite me.
g0 = (take 8 $ repeat $ take 21 $ repeat 0) ++ [(take 10 $ repeat 0)++    [1]++(take 10 $ repeat 0),
                                                (take  9 $ repeat 0)++[1,1,1]++(take  9 $ repeat 0),
                                                (take  9 $ repeat 0)++[1,0,1]++(take  9 $ repeat 0),
                                                (take 10 $ repeat 0)++    [1]++(take 10 $ repeat 0)] ++
     (take 8 $ repeat $ take 21 $ repeat 0) :: [[Int]]

type Point = (Int, Int)
type PoFunc = (Int -> Int, Int -> Int) 
type Grid = [[Int]]

apply :: PoFunc -> Point -> Point
apply (f1,f2) (r1,r2) = (f1 r1,f2 r2)

--filters remove negatives and identities
neighborP :: [PoFunc] -> Point -> [Point]
neighborP fs p = filter (\p2@(_,_) -> p /= p2) $ filter (\(r1,r2) -> r1 >= 0 && r2 >= 0) $ (\f -> apply f p) <$> fs

get :: Grid -> Point -> Int
get xxs (r1,r2)
    |r1 >= length xxs = 0
    |r2 >= length (xxs!!r1) = 0
    |otherwise = (xxs!!r1)!!r2

nsum :: Grid -> Point -> Int
nsum xxs p = sum $ (get xxs) <$> (neighborP ogrid p)

toVal :: Int -> Int -> Int
toVal o n
    | n < 2 || n > 3 = 0
    | n == 3 = 1
    | o == 1 = 1
    | otherwise = 0
evolve :: Grid -> Grid
evolve xxs = [ [ toVal (get xxs (x,y)) (nsum xxs (x,y)) | y <- [0..100], y < length (xxs!!x)] | x <- [0..100], x < length xxs]

--Ideally, I'd have used ncurses or something like that
--But I was too lazy, lel
printGrid g = let printCpt 1 = putStr "■ "
                  printCpt 0 = putStr ". "
              in mapM_ (\xs -> do
                            mapM_ printCpt xs
                            putStr "\n") g
simulate :: Grid -> IO ()
simulate g = do
    printGrid g;
    let g2 = evolve g;
    threadDelay 500000;
    simulate g2

main = simulate g0
