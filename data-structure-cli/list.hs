import Control.Applicative

-- todo: generalize
type State = [String]

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a          -> ST b
    -- fmap :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))
    fmap g (S st) = S(\s -> let (x, s') = st s in (g x, s'))

instance Applicative ST where
    -- pure :: a -> ST a
    -- pure :: a -> (s -> (a, s))
    pure x = S(\s -> (x, s))

    -- (<*>) :: ST (a -> b)                 -> ST a          -> ST b
    -- (<*>) :: (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s))
    stf <*> stx = S(\s -> let (f, s') = app stf s
                              (x, s'') = app stx s' in (f x, s''))

instance Monad ST where
    return = pure
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S(\s -> let (x, s') = app st s in app (f x) s')

add :: String -> ST [String]
add x = S(\ys -> (x:ys,x:ys))

remove :: String -> ST [String]
remove x = S(\xs -> let ys = filter (/=x) xs in (ys,ys))
           

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = part p ([],[]) xs
                 where part p (ts,fs)     [] = (ts,fs)
                       part p (ts,fs) (x:xs) = if p x then part p (x:ts,fs) xs else part p (ts,x:fs) xs

list :: [String] -> IO [String]
list state = do putStr "Enter command ('add' or 'remove'): "
                command <- getLine
                case command of
                    "add"    -> do element <- getLine
                                   putStr "Enter new element: "
                                   new <- add element state
                                   putStr "New state: " ++ new
                                   list new
                    "remove" -> do element <- getLine
                                   putStr "Enter element to remove: "
                                   new <- remove element state
                                   putStr "New state: " ++ new
                                   list new
                    _        -> do putStr "Unsupported command."
                                   list state

run :: IO()
run = list []
