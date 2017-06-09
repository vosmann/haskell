module Main where

import State.StateMonad

main :: IO ()
main = run []

run :: [String] ->  IO ()
run s = do putStrLn "Command"
           command <- getLine
           putStrLn "Element: "
           element <- getLine
           let s' = mutate command element s
           putStrLn "New state: "
           putStrLn (show s')
           run s'

mutate cmd elem s = execState statemonad s 
                      where statemonad = case cmd of
                                             "add"    -> modify (elem:)
                                             "remove" -> modify (filter (/=elem))
                                             _        -> put s
