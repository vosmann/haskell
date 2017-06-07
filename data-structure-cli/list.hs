add :: String -> ST [String]
add x (S st) = S(\xs -> ([x], x:xs))

remove :: String -> ST [String]
remove x = S(\xs -> let (removed,left) = partition (==x) xs in (removed,left))


list :: [String] -> IO [String]
list state = do putStr "Enter command ('add' or 'remove'): "
                command <- getLine
                case command of
                    "add"    -> do element <- getLine
                                   putStr "Enter new element: "
                                   new <- add element state
                                   putStrLn "New state: "
                                   putStrLn new
                                   list new
                    "remove" -> do element <- getLine
                                   putStr "Enter element to remove: "
                                   new <- remove element state
                                   putStrLn "New state: "
                                   putStrLn new
                                   list new
                    _        -> do putStrLn "Unsupported command."
                                   list state

-- state can be threaded through recursive calls, no state monad necessary
run :: IO [String]
run = list []


partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = part p ([],[]) xs
                 where part p (ts,fs)     [] = (ts,fs)
                       part p (ts,fs) (x:xs) = if p x then part p (x:ts,fs) xs else part p (ts,x:fs) xs


