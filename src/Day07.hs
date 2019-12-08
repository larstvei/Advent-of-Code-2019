module Day07 where
import Day05 hiding (main)
import Control.Monad
import Data.List (permutations)
import Debug.Trace
import Safe
import qualified Data.Map as M

execPhase :: Int -> M.Map Int Int -> [Int] -> Maybe Int
execPhase input tape ps = do
  p <- headMay ps
  a1 <- M.lookup 1 tape
  res <- exec 2 input [] $ M.insert a1 p tape
  output <- M.lookup (-1) res
  case tailMay ps of
    Just [] -> return output
    Just ps -> execPhase output tape ps

solveA :: M.Map Int Int -> Maybe Int
solveA tape = do
  let phases = permutations [0..4]
  res <- mapM (execPhase 0 tape) phases
  return $ maximum res

main :: IO ()
main = do
    contents <- readFile "input/07"
    let parsed = parse contents

    print $ solveA parsed

    return ()
