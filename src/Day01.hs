module Day01 where

parse :: String -> [Int]
parse = fmap read . lines

fuel :: Int -> Int
fuel = subtract 2 . (flip div) 3

solveA :: [Int] -> Int
solveA = sum . fmap fuel

solveB :: [Int] -> Int
solveB = sum . fmap (sum . takeWhile (>0) . tail . iterate fuel)

main :: IO ()
main = do
    contents <- readFile "input/01"
    let parsed = parse contents

    print $ solveA parsed
    print $ solveB parsed
