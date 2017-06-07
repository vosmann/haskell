module Main where

import State.StateMonad

{-
add :: String -> ST [String]
add x (S st) = S(\xs -> ([x], x:xs))

remove :: String -> ST [String]
remove x = S(\xs -> let (removed,left) = partition (==x) xs in (removed,left))
-}

list :: IO ()
list = do putStr "Enter command ('add' or 'remove'): "
          command <- getLine
          case command of
                    "add"    -> do putStr "(Add) Enter element: "
                                   element <- getLine
                                   putStrLn "New element: "
                                   putStrLn element
                                   putStrLn "New state: "
                                   putStrLn "fake state after addition"
                                   list
                    "remove" -> do putStr "(Remove) Enter element: "
                                   element <- getLine
                                   putStrLn "Removed element: "
                                   putStrLn element
                                   putStrLn "New state: "
                                   putStrLn "fake state after removal"
                                   list
                    _        -> do putStrLn "Unsupported command."
                                   list



partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = part p ([],[]) xs
                 where part p (ts,fs)     [] = (ts,fs)
                       part p (ts,fs) (x:xs) = if p x then part p (x:ts,fs) xs else part p (ts,x:fs) xs


