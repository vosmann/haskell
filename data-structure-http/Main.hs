{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Prelude hiding (map, concat, putStrLn)

import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Data.ByteString -- Strict.
import Data.ByteString.Lazy (fromChunks)

import Control.Monad (join)

default (ByteString)

main :: IO ()
main = do putStrLn $ "Listening on port 8080"
          listRef <- newIORef []
          run 8080 (app listRef)

-- app :: IORef [a] -> (Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived)
app :: IORef [ByteString] -> Application
app listRef request respond = do putStrLn $ logged request
                                 let cmd = param request "command"
                                     elem = param request "element"
                                 list <- modify cmd elem listRef
                                 putStrLn $ concat list
                                 respond $ responseLBS status200
                                                       [("Content-Type", "text/plain")]
                                                       (fromChunks list)

logged req = concat [requestMethod req,
                     rawPathInfo req,
                     rawQueryString req]

-- queryString returns [(ByteString, Maybe ByteString)], i.e. strict, not lazy.
param req name = join (lookup name (queryString req))

modify :: Maybe ByteString -> Maybe ByteString -> IORef [ByteString] -> IO [ByteString]
modify cmd elem list = atomicModifyIORef list (\s -> (op s, op s))
                       where op = selectOperation cmd elem 

selectOperation :: Maybe ByteString -> Maybe ByteString -> ([ByteString] -> [ByteString])
selectOperation (Just "add") (Just elem) = (\xs -> elem:xs)
selectOperation (Just "rem") (Just elem) = (\xs -> Prelude.filter (/=elem) xs)
selectOperation _            _           = id
