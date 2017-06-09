module Main where

import State.StateMonad

import Data.Set (Set)
import qualified Data.Set as Set

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

--  Data structure (list, set, hash map)?
--  Command?
--  Element?

main :: IO ()
main = run []

run :: [String] ->  IO ()
run s = do command <- collectLn "Command: "
           element <- collectLn "Element: "
           let s' = mutate command element s
           putStrLn $ "  State: " ++ (show s')
           run s'

{-
run' :: Set String ->  IO ()
run' s = do command <- collectLn "Command: "
            element <- collectLn "Element: "
           let s' = mutate command element s
           putStrLn $ "  State: " ++ (show s')
            run' s
-}

collectLn :: String -> IO String
collectLn text = do putStr text
                    getLine

mutate :: Eq a => String -> a -> [a] -> [a]
mutate cmd elem s = execState statemonad s 
                      where f = case cmd of
                                    "add" -> (elem:)
                                    "rem" -> filter (/=elem)
                                    _     -> id
                            statemonad = modify f

{-
-- Would work if the b type would be abstracted to a monoid that also supports a remove operation.
-- stateModifiers :: Eq a => [(String, [(String, a -> b)])]
stateModifiers = [("list", [("add", (:)),
                            ("rem", \x -> filter (/=x))]),
                  ("set",  [("add", ),
                            ("rem", )]),
                  ("map",  [("add", ),
                            ("rem", )])]
-}

