{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid (mconcat)
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Control.Monad.Trans (liftIO, lift)
import Data.Text.Lazy (Text)


main = scotty 3000 $ app $ newIORef []

-- use scotty and try putting in a IORef

{-
$ do post "/list" $ do cmd <- param "command"
                       elem <- param "element"
                       html $ mconcat ["<h1>", cmd, elem, "</h1>"]
-}

app sharedState = do post "/list" $ do cmd <- param "command"
                                       elem <- param "element"
                                       list <- lift $ atomicModifyIORef sharedState (\s -> mutate' cmd elem s)
                                       html $ mconcat ["<h1>", cmd, elem, "</h1>"]


mutate :: Eq a => String -> a -> [a] -> [a]
mutate cmd elem s = case cmd of
                        "add" -> (elem:s)
                        "rem" -> filter (/=elem) s
                        _     -> s

mutate' :: Eq a => Text -> a -> [a] -> ([a], Int)
mutate' cmd elem s = (case cmd of
                         "add" -> (elem:s)
                         "rem" -> filter (/=elem) s
                         _     -> s,
                      100)

