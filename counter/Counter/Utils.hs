module Counter.Utils (
    eventFromList,
    splitOn,
    splitOnFirstSemicolon,
    splitOnFirst 
) where


import Data.List
import Data.Maybe


-- Event
type Tag = String
type Date = String
type Description = String

data Event = Event String String String
           | FaultyEvent String
           deriving Show

tag :: Event -> Tag
tag (Event t _ _) = t
tag (FaultyEvent desc) = "unknown-tag"

date :: Event -> Date
date (Event _ d _) = d
date (FaultyEvent desc) = "unknown-date"

description :: Event -> Date
description (Event _ _ desc) = desc
description (FaultyEvent desc) = desc

eventFromList :: [String] -> Event
eventFromList strings 
    | length strings == 3 = Event (first strings) (second strings) (third strings)
    | otherwise = FaultyEvent (concat strings)

-- toTriplet :: [String] -> (String, String, String)
-- toTriplet [x, y, z] = (x, y, z)

first :: [a] -> a
first [x, _, _] = x

second :: [a] -> a
second [_, y, _] = y

third :: [a] -> a
third [_, _, z] = z


-- Record syntac approach:
--data Event = Event { tag :: Tag
--                   , description :: Description
--                   , date :: Date
--                   } deriving (Eq, Show)

                        
-- Splitting logic 2 (meh)
-- Recognize
-- Could be more generic like this: splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn :: Char -> [Char] -> [[Char]]
splitOn delim = foldr split [[]] -- why not just []
    where split char all@(x:xs) | char == delim = []:all
                                | otherwise = (char:x):xs


-- Splitting logic 1 (not performant)
splitOnFirstSemicolon :: [Char] -> ([Char], [Char])
splitOnFirstSemicolon = splitOnFirst ';'

-- First looks for the first index of delim, then splits, if one is found.
splitOnFirst :: Char -> [Char] -> ([Char], [Char])
splitOnFirst delim str 
    | fstIndex == Nothing = ("unknown", str)
    | otherwise           = removeFstCharFromSecond $ splitAt (fromJust fstIndex) str
    where fstIndex = elemIndex delim str

removeFstCharFromSecond :: ([Char], [Char]) -> ([Char], [Char])
removeFstCharFromSecond (a, b) = (a, tail b)

