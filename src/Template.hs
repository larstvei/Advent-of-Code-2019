module DayXX where

parse :: String -> a
parse = undefined

solveA = undefined

solveB = undefined

main :: IO ()
main = do
    contents <- readFile "input/XX"
    let parsed = parse contents

    -- print $ solveA parsed
    -- print $ solveB parsed

    return ()
