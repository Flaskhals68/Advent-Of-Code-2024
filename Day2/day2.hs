import System.IO (readFile)

---------------- Input parsing ----------------

readReports :: FilePath -> IO [[Int]]
readReports fp = do
    content <- readFile fp
    let reports = map formatRow $ lines content
    return reports
        where
            formatRow r = map read $ words r


-------------------- Part 1 --------------------

printSafeCount :: [[Int]] -> IO ()
printSafeCount reports = do
    let count = safeCount reports
    putStrLn $ "Safe reports: " ++ show count

safeCount :: [[Int]] -> Int
safeCount = length . filter isSafe
    where
        isSafe row = (isAscending row || isDescending row) && hasSafeDiffs row
        isAscending = hasProperty (<)
        isDescending = hasProperty (>)
        hasSafeDiffs = hasProperty (\x y -> let diff = abs (x - y) in diff >= 1 && diff <= 3)

hasProperty :: (Int -> Int -> Bool) -> [Int] -> Bool
hasProperty p xs = all (uncurry p) (zip xs (tail xs))


main :: IO ()
main = do
    reports <- readReports "input.txt"
    printSafeCount reports