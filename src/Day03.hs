module Day03 where
import Control.Arrow
import Control.Lens
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char (isDigit)

data Segment = L Int | R Int | D Int | U Int deriving (Read, Show)

parseWire :: String -> [Segment]
parseWire = fmap (\(dir:n) -> read $ dir:' ':n) . splitOn ","

parse :: String -> [[Segment]]
parse = fmap parseWire . lines

points :: [Segment] -> [(Int, Int)]
points = reverse . foldl f [(0, 0)]
    where f (p:ps) seg = (g seg p) ++ ps
          g (L l) = h l (over _1 pred)
          g (R l) = h l (over _1 succ)
          g (D l) = h l (over _2 pred)
          g (U l) = h l (over _2 succ)
          h l step = reverse . take (l+1) . iterate step

intersectionPoints :: [Segment] -> [Segment] -> S.Set (Int, Int)
intersectionPoints w1 w2 = S.intersection
                           (S.fromList $ points w1)
                           (S.fromList $ points w2)

solveA w1 w2 = S.findMin $ S.deleteMin $ S.map (\(a,b) -> abs a + abs b) ps
    where ps = intersectionPoints w1 w2

solveB w1 w2 = S.findMin $ S.map time ps
    where ps1 = points w1
          ps2 = points w2
          m1 = foldl insert M.empty $ zip ps1 [0..]
          m2 = foldl insert M.empty $ zip ps2 [0..]
          insert m (k, v) = M.insertWith min k v m
          ps = S.delete (0,0) $ intersectionPoints w1 w2
          time k = (+) <$> M.lookup k m1 <*> M.lookup k m2

main :: IO ()
main = do
    contents <- readFile "input/03"
    let [w1, w2] = parse contents

    print $ solveA w1 w2
    print $ solveB w1 w2

    return ()
