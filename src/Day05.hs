module Day02 where
import Control.Lens
import Control.Monad
import Data.SBV
import Data.SBV.Control
import Data.List.Split (splitOn)
import Debug.Trace
import qualified Data.Map as M

parse :: String -> M.Map Int Int
parse = M.fromList . zip [0..] . fmap read . splitOn ","

jump True b _ = b
jump False _ pos = pos + 3

exec :: Int -> Int -> [Int] -> M.Map Int Int -> Maybe (M.Map Int Int)
exec pos input outputs tape = do
  inst <- M.lookup pos tape
  let iMode = flip M.lookup tape
      pMode = (iMode >=> iMode)
      op = inst `rem` 100
      m1 | (inst `div` 100) `rem` 10 == 1 = iMode
         | otherwise = pMode
      m2 | (inst `div` 1000) `rem` 10 == 1 = iMode
         | otherwise = pMode
  case op of
    99 -> return $ M.insert (-1) (head outputs) tape
    otherwise -> do
      a <- m1 (pos + 1)
      ai <- iMode (pos + 1)
      case op of
        3 -> exec (pos + 2) input outputs $ M.insert ai input tape
        4 -> exec (pos + 2) input (a : outputs) tape
        otherwise -> do
          b <- m2 (pos + 2)
          case op of
            5 -> exec (jump (a > 0) b pos) input outputs tape
            6 -> exec (jump (a == 0) b pos) input outputs tape
            otherwise -> do
              c <- iMode (pos + 3)
              case op of
                1 -> exec (pos + 4) input outputs $ M.insert c (a + b) tape
                2 -> exec (pos + 4) input outputs $ M.insert c (a * b) tape
                7 -> exec (pos + 4) input outputs $ M.insert c (fromEnum $ a < b) tape
                8 -> exec (pos + 4) input outputs $ M.insert c (fromEnum $ a == b) tape
                otherwise -> Nothing

solveA = exec 0 1 []

solveB = exec 0 5 []

main :: IO ()
main = do
    contents <- readFile "input/05"
    let parsed = parse contents

    print $ join $ M.lookup (-1) <$> solveA parsed
    print $ join $ M.lookup (-1) <$> solveB parsed
