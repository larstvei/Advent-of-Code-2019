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

exec :: Int -> [Int] -> M.Map Int Int -> Maybe (M.Map Int Int)
exec pos outputs tape = do
  let iMode = flip M.lookup tape
      pMode = (iMode >=> iMode)
  inst <- iMode pos
  let op = inst `rem` 100
      m1 = if (inst `div` 100) `rem` 10 == 1 then iMode else pMode
      m2 = if (inst `div` 1000) `rem` 10 == 1 then iMode else pMode
  case op of
    99 -> return $ M.insert (-1) (head outputs) tape
    otherwise -> do
      a <- m1 (pos + 1)
      ai <- iMode (pos + 1)
      case op of
        3 -> exec (pos + 2) outputs $ M.insert ai 1 tape
        4 -> exec (pos + 2) (a : outputs) tape
        otherwise -> do
          b <- m2 (pos + 2)
          c <- iMode (pos + 3)
          case op of
            1 -> exec (pos + 4) outputs $ M.insert c (a + b) tape
            2 -> exec (pos + 4) outputs $ M.insert c (a * b) tape
            otherwise -> Nothing

solveA = exec 0 []

main :: IO ()
main = do
    contents <- readFile "input/05"
    let parsed = parse contents

    print $ join $ M.lookup (-1) <$> solveA parsed