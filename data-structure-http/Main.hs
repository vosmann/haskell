{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Prelude hiding (map, concat, putStrLn)

import Data.IORef (IORef, newIORef, atomicModifyIORef)
--import Data.ByteString.Lazy.Char8 (pack, unpack)
-- Wai uses strict ByteStrings.
import Data.ByteString
import Data.ByteString.Lazy (fromChunks)
import Control.Monad (join)
import Control.Monad.Trans (liftIO, lift)

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
                                 list <- inc cmd elem listRef
                                 putStrLn $ concat list -- show for bytestrings?
                                 respond $ responseLBS status200
                                                       [("Content-Type", "text/plain")]
                                                       (fromChunks list)

-- show for bytestrings?
logged req = concat [requestMethod req,
                     rawPathInfo req,
                     rawQueryString req]

-- queryString returns [(ByteString, Maybe ByteString)], i.e. strict ByteStrings.
param req name = join (lookup name (queryString req))

inc :: Maybe ByteString -> Maybe ByteString -> IORef [ByteString] -> IO [ByteString]
inc cmd elem = modify f
               where f = chooseMod cmd elem 
                     modify f list = atomicModifyIORef list (\s -> (f s, f s))

chooseMod :: Maybe ByteString -> Maybe ByteString -> ([ByteString] -> [ByteString])
chooseMod (Just "add") (Just elem) = (\xs -> elem:xs)
chooseMod (Just "rem") (Just elem) = (\xs -> Prelude.filter (/=elem) xs)
chooseMod _            _           = id
