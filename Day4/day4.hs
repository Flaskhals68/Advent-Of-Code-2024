import Data.List (transpose, sort)

-------------------- Part 1 --------------------

xmasCount :: String -> Int
xmasCount input = sum (map count rows) + sum (map count columns) + sum (map count diags)
    where 
        rows = lines input
        columns = transpose rows
        diags = allDiagonals rows
        count block 
            | length (take 4 block) < 4 = 0
            | otherwise = isXmas (take 4 block) + count (drop 1 block)
        isXmas s 
            | s == "XMAS" || s == "SAMX" = 1
            | otherwise                  = 0


allDiagonals :: [[a]] -> [[a]]
allDiagonals xss = diagonals xss ++ diagonals (rotate90 xss)
    where
        diagonals xss = [[xss !! i !! (j - i) | i <- [0..j], i < length xss, (j - i) < length (xss !! i)] | j <- [0..2 * length xss - 2]]
        rotate90 = reverse . transpose


-------------------- Part 2 --------------------

xmasCountPart2 :: String -> Int
xmasCountPart2 s = sum $ map isXmas (map removeEveryOther squares)
    where
        rows = lines s
        squares = toSquares rows
        isXmas s
            | s `elem` ["MSAMS", "SMASM", "MMASS", "SSAMM"]  = 1
            | otherwise                                      = 0


removeEveryOther :: [a] -> [a]
removeEveryOther [] = []
removeEveryOther [x] = [x]
removeEveryOther (x:_:xs) = x : removeEveryOther xs


toSquares :: [[a]] -> [[a]]
toSquares [] = []
toSquares rows
    | length rows < 3 = []
    | otherwise =
        let (r1:r2:r3:rest) = rows 
        in  concat3x3 r1 r2 r3 ++ toSquares (tail rows)


concat3x3 :: [a] -> [a] -> [a] -> [[a]]
concat3x3 row1 row2 row3 =
    map (\(c1, c2, c3) -> concat [c1, c2, c3]) $ zip3 (chunksOf3 row1) (chunksOf3 row2) (chunksOf3 row3)


chunksOf3 :: [a] -> [[a]]
chunksOf3 [] = []
chunksOf3 xs
    | length xs < 3 = []
    | otherwise = take 3 xs : chunksOf3 (drop 1 xs)


main :: IO ()
main = do
    content <- readFile "input.txt"
    print ("Part 1: " ++ show (xmasCount content))
    print ("Part 2: " ++ show (xmasCountPart2 content))
