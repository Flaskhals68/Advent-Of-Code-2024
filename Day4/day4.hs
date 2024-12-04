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


main :: IO ()
main = do
    content <- readFile "input.txt"
    print (xmasCount content)