-- import System.IO
-- main :: IO ()
-- main = do
    --withFile "counter.csv" AppendMode (\handle -> do 
        --contents <- hGetContents handle
        --putStr contents)

module Main where

import Counter.Utils

main = do
    line <- getLine
    let entry = splitOnFirstSemicolon line
        tag = fst entry
        value = snd entry
        event = eventFromList (splitOn ';' line)

    appendFile "counter.csv" $ tag ++ ";" ++ value ++ "\n"
    putStrLn $ "tag: " ++ tag ++ ", value: " ++ value ++ ", full line: " ++ show event


