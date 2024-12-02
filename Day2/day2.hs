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


isSafe :: [Int] -> Bool
isSafe row = (isAscending row || isDescending row) && hasSafeDiffs row
    where
        isAscending = hasProperty (<)
        isDescending = hasProperty (>)
        hasSafeDiffs = hasProperty (\x y -> let diff = abs (x - y) in diff >= 1 && diff <= 3)


hasProperty :: (Int -> Int -> Bool) -> [Int] -> Bool
hasProperty p xs = all (uncurry p) (zip xs (tail xs))


-------------------- Part 2 --------------------

printSafeDampenedCount :: [[Int]] -> IO ()
printSafeDampenedCount reports = do
    let count = safeDampedCount reports
    putStrLn $ "Safe dampened reports: " ++ show count


safeDampedCount :: [[Int]] -> Int
safeDampedCount = length . filter isSafeDampened
    where
        isSafeDampened r = any (isSafe . (`deleteAt` r)) [0..length r - 1]


deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = let (before, after) = splitAt idx xs
            in before ++ (if null after then [] else tail after)


main :: IO ()
main = do
    reports <- readReports "input.txt"
    printSafeCount reports
    printSafeDampenedCount reports