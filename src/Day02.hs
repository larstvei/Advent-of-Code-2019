module Day02 where
import Control.Lens
import Control.Monad
import Data.SBV
import Data.SBV.Control
import Data.List.Split (splitOn)
import qualified Data.Map as M

parse :: String -> M.Map Int Int
parse = M.fromList . zip [0..] . fmap read . splitOn ","

exec :: Int -> M.Map Int Int -> Maybe (M.Map Int Int)
exec pos tape = do
  a <- M.lookup (pos+1) tape >>= flip M.lookup tape
  b <- M.lookup (pos+2) tape >>= flip M.lookup tape
  c <- M.lookup (pos+3) tape
  case M.lookup pos tape of
    Just 1 -> exec (pos+4) (M.insert c (a+b) tape)
    Just 2 -> exec (pos+4) (M.insert c (a*b) tape)
    Just 99 -> return tape
    otherwise -> Nothing

solveA = exec 0 . M.insert 2 2 . M.insert 1 12

solveB :: SInteger -> SInteger -> SBool
solveB noun verb = 1114711 + noun*216000 + verb .== 19690720
                   .&& verb .< noun

main :: IO ()
main = do
    contents <- readFile "input/02"
    let parsed = parse contents

    print $ join $ M.lookup 0 <$> solveA parsed

    -- By testing a few instances for solveB, we quickly see that the output is
    -- increased by 216000 whenever the noun is increased by one, and increased
    -- by one whenever the verb is increased by one. The output is 1114711 when
    -- both the noun and the verb is 0. Solving the equation
    --
    --     1114711 + noun*216000 + verb = 19690720
    --
    -- over the integers, with the additional requirement of the verb being
    -- smaller than the noun gives the solution (see also extra/day02.smt2).
    -- The additional requirement is just to get reasonably small numbers.

    res <- sat solveB
    let m = extractModel res :: Maybe (Integer, Integer)
    print $ uncurry (+) <$> over _1 (*100) <$> m
