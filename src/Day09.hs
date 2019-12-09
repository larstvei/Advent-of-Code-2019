module Day09 where
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.List.Split (splitOn)
import Debug.Trace
import Safe
import qualified Data.Map as M

parse :: String -> M.Map Int Int
parse = M.fromList . zip [0..] . fmap read . splitOn ","

interpretParams :: Int -> Int -> Int -> M.Map Int Int -> (Int, Int, Int)
interpretParams base pos inst tape =
    let [a, b, c] = [(inst `div` 10^d) `rem` 10 | d <- [2..4]]
        look = flip (M.findWithDefault 0) tape
        read 0 = look . look
        read 1 = look
        read 2 = look . (+base) . look
        write 0 = look
        write 2 = (+base) . look
    in case inst `rem` 100 of
      3 -> (write a $ pos + 1, read b $ pos + 2, write c $ pos + 3)
      _ -> (read  a $ pos + 1, read b $ pos + 2, write c $ pos + 3)

exec :: Int -> Int -> Int -> [Int] -> M.Map Int Int -> Maybe (M.Map Int Int)
exec base pos input outputs tape = do
  inst <- M.lookup pos tape
  let put k v = M.insert k v tape
      (a, b, c) = interpretParams base pos inst tape
  case inst `rem` 100 of
    1 -> exec base (pos + 4) input outputs $ put c (a + b)
    2 -> exec base (pos + 4) input outputs $ put c (a * b)
    3 -> exec base (pos + 2) input outputs $ put a input
    4 -> exec base (pos + 2) input (a : outputs) tape
    5 -> exec base (bool (pos + 3) b (a > 0)) input outputs tape
    6 -> exec base (bool (pos + 3) b (a == 0)) input outputs tape
    7 -> exec base (pos + 4) input outputs $ put c (fromEnum $ a < b)
    8 -> exec base (pos + 4) input outputs $ put c (fromEnum $ a == b)
    9 -> exec (base+a) (pos + 2) input outputs tape
    99 -> put (-1) <$> headMay outputs
    _ -> (trace $ show outputs) Nothing

main :: IO ()
main = do
    contents <- readFile "input/09"
    let parsed = parse contents

    print $ M.lookup (-1) =<< exec 0 0 1 [] parsed
    print $ M.lookup (-1) =<< exec 0 0 2 [] parsed
