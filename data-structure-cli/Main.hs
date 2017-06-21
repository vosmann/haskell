module Main where

--import State.StateMonad
import Control.Monad.State

--import Data.Set (Set)
--import qualified Data.Set as Set

--import Data.HashMap.Strict (HashMap)
--import qualified Data.HashMap.Strict as HashMap

--         Support:
--         * list        -- DONE
--           * add x     -- DONE
--           * remove x  -- DONE
--         * set
--           * add x
--           * remove x
--         * hashmap
--           * add k v
--           * remove k v
--           * use Hashable to autogenerate k

main :: IO ()
main = run []

collectLn :: String -> IO String
collectLn text = do putStr text
                    getLine

-- With a state monad.
run :: [String] ->  IO ()
run s = do command <- collectLn "Command: "
           element <- collectLn "Element: "
           let s' = mutate command element s
           putStrLn $ "  State: " ++ (show s')
           run s'

mutate :: Eq a => String -> a -> [a] -> [a]
mutate cmd elem s = execState statemonad s 
                      where f = case cmd of
                                    "add" -> (elem:)
                                    "rem" -> filter (/=elem)
                                    _     -> id
                            statemonad = modify f

-- Without a state monad.
runWithoutStateMonad :: [String] ->  IO ()
runWithoutStateMonad s = do putStrLn $ "  State: " ++ (show s)
                            command <- collectLn "Command: "
                            element <- collectLn "Element: "
                            let s' = mutateWithoutStateMonad command element s
                            runWithoutStateMonad s'


mutateWithoutStateMonad :: Eq a => String -> a -> [a] -> [a]
mutateWithoutStateMonad cmd elem s = case cmd of
                                         "add" -> (elem:s)
                                         "rem" -> filter (/=elem) s
                                         _     -> s

-- With a monad transformer.
runLoop s = execStateT runWithMonadTransformer s

runWithMonadTransformer :: StateT [String] IO ()
runWithMonadTransformer = do command <- lift $ collectLn "Command: "
                             element <- lift $ collectLn "Element: "
                             statemonad command element
                             s <- get
                             lift $ putStrLn $ "  State: " ++ (show s)
                             runWithMonadTransformer

statemonad :: Eq a => String -> a -> StateT [a] IO ()
statemonad cmd elem = modify f
                          where f = case cmd of
                                        "add" -> (elem:)
                                        "rem" -> filter (/=elem)
                                        _     -> id

{-
x :: Set Int
x = Set.fromList [1,2,3,4]
y :: Set Int
y = Set.insert 4 $ Set.insert 5 $ Set.empty
-}
