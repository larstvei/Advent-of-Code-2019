module Day06 where
import Data.List ((\\))
import Data.Tuple (swap)
import Data.List.Split (splitOn)
import Control.Lens
import Debug.Trace
import qualified Data.Map as M

parse :: String -> [[String]]
parse = fmap (splitOn ")") . lines

solveA :: Int -> M.Map String [String] -> String -> Int
solveA i m k = (+i) $ sum $ fmap (solveA (succ i) m) neighbors
    where neighbors = M.findWithDefault [] k m

solveB i v m "SAN" = i
solveB i v m k = minimum $ ((:) t)
               $ fmap (solveB (succ i) (k:v) m) neighbors
    where neighbors = M.findWithDefault [] k m \\ v
          t = (maxBound::Int) `div` 2

main :: IO ()
main = do
    contents <- readFile "input/06"
    let parsed = parse contents
        tuples = fmap (\[a, b] -> (a, b)) parsed
        m1 = M.fromListWith (++) $ fmap (over _2 (:[])) tuples
        m2 = M.fromListWith (++) $ fmap ((over _2 (:[])) . swap) tuples

    print $ solveA 0 m1 "COM"
    print $ subtract 2 $ solveB 0 [] (M.unionWith (++) m1 m2) "YOU"
