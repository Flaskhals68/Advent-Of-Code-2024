import System.IO (readFile)
import Data.List (unzip, sort, group, intersect)
import Text.Read (readMaybe)


---------------- Input parsing ----------------

parseFile :: FilePath -> IO ([Int], [Int])
parseFile path = do
    content <- readFile path
    let (as, bs) = unzip $ map parseLine (lines content)
    return (as, bs)
    where
        parseLine line = case words line of
            [x, y] -> (read x, read y)
            _      -> error "Invalid file format"


-------------------- Part 1 --------------------

printDistance :: [Int] -> [Int] -> IO ()
printDistance a b = do
    let distance = totalDistance a b
    putStrLn $ "Total distance: " ++ (show distance)


totalDistance :: [Int] -> [Int] -> Int
totalDistance a b = sum $ map (\(x,y) -> abs(x-y)) $ zip sa sb
    where 
        sa = sort a
        sb = sort b


-------------------- Part 2 --------------------

printSimilarityScore :: [Int] -> [Int] -> IO ()
printSimilarityScore a b = do
    let score = similarityScore a b
    putStrLn $ "Similarity score: " ++ (show score)


similarityScore :: [Int] -> [Int] -> Int
similarityScore a b = score filteredAFreq filteredBFreq
    where
        aFreq = freqMap a
        bFreq = freqMap b
        
        freqMap xs = map (\ys -> (head ys, length ys)) . group . sort $ xs
        
        commonKeys = map fst aFreq `intersect` map fst bFreq
        
        filteredAFreq = filter (\(k, _) -> k `elem` commonKeys) aFreq
        filteredBFreq = filter (\(k, _) -> k `elem` commonKeys) bFreq
        
        score ((a1,a2):as) ((b1,b2):bs) = (a1 * b2) + (score as bs)  
        score _ _                       = 0 


main :: IO ()
main = do
    (a, b) <- parseFile "input.txt"
    printDistance a b
    printSimilarityScore a b
