module Day05 where
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.List.Split (splitOn)
import Safe
import qualified Data.Map as M

parse :: String -> M.Map Int Int
parse = M.fromList . zip [0..] . fmap read . splitOn ","

digitAtFromRight :: Int -> Int -> Int
digitAtFromRight n d = (n `div` 10^d) `rem` 10

exec :: Int -> Int -> [Int] -> M.Map Int Int -> Maybe (M.Map Int Int)
exec pos input outputs tape = do
  inst <- M.lookup pos tape
  let put k v = M.insert k v tape
      iMode = flip M.lookup tape
      pMode = (iMode >=> iMode)
      a = bool iMode pMode (digitAtFromRight inst 2 == 0) $ (pos+1)
      b = bool iMode pMode (digitAtFromRight inst 3 == 0) $ (pos+2)
      c = iMode (pos+3)
  case digitAtFromRight inst 0 of
    1 -> put <$> c <*> ((+) <$> a <*> b) >>= exec (pos + 4) input outputs
    2 -> put <$> c <*> ((*) <$> a <*> b) >>= exec (pos + 4) input outputs
    3 -> flip put input <$> iMode (pos + 1) >>= exec (pos + 2) input outputs
    4 -> do o <- a ; exec (pos + 2) input (o : outputs) tape
    5 -> do x <- a ; y <- b ; exec (bool (pos + 3) y (x > 0)) input outputs tape
    6 -> do x <- a ; y <- b ; exec (bool (pos + 3) y (x == 0)) input outputs tape
    7 -> put <$> c <*> (fromEnum <$> ((<) <$> a <*> b)) >>= exec (pos + 4) input outputs
    8 -> put <$> c <*> (fromEnum <$> ((==) <$> a <*> b)) >>= exec (pos + 4) input outputs
    9 -> put (-1) <$> headMay outputs

main :: IO ()
main = do
    contents <- readFile "input/05"
    let parsed = parse contents

    print $ M.lookup (-1) =<< exec 0 1 [] parsed
    print $ M.lookup (-1) =<< exec 0 5 [] parsed
