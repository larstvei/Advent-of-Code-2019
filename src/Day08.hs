module Day08 where
import Data.Ord
import Data.List
import Data.List.Split (chunksOf)

parse :: Int -> Int -> String -> [[Int]]
parse h w = chunksOf (h*w) . fmap (read . pure)

solveA = product
         . fmap length
         . take 2 . tail . head
         . sortBy (comparing $ length . head)
         . fmap (group . sort)

solveB :: [[Int]] -> [Int]
solveB = foldr1 combine
    where combine l1 l2 = fmap (uncurry choose) $ zip l1 l2
          choose 2 x = x
          choose x _ = x

pretty w h = unlines . chunksOf w . fmap f
    where f 0 = '⬜'
          f 1 = '⬛'
          f 2 = '?'             -- shouldn't happen

main :: IO ()
main = do
    contents <- readFile "input/08"
    let (width, height) = (25, 6)
        parsed = parse width height contents

    print $ solveA parsed
    putStr $ pretty width height $ solveB parsed

    return ()
