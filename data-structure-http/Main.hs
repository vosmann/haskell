{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Data.ByteString.Lazy.Char8 (pack)
import Control.Monad.Trans (liftIO, lift)

{-
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString.Lazy as B (concat, ByteString, append)
import Data.ByteString.Lazy.UTF8 (fromString)

application :: (Num a, Show a) => IORef a -> Request -> ResourceT IO Response
application counter request = do  -- (3)
    count <- lift $ incCount counter  -- (5)
    liftIO $ printCount count -- (6)
    let responseText = 
    return $ make

makeResponse = responseLBS status200 [("Content-type", "text/html")] $ responseText -- (7), (8)
(makeResponseText count)

makeResponseText :: (Show a) => a -> B.ByteString
makeResponseText s = "<h1>Hello World " `append` (toByteString s) `append` "</h1>\n"

toByteString :: (Show a) => a -> B.ByteString
toByteString s = fromString $ show s


main = do 
    counter <- newIORef 0 -- (1)
    run 3000 $ application counter -- (2)
-}

-- app :: IORef [a] -> (Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived)
app :: IORef [String] -> Application
app list request respond = do let params = queryString request
                              let respString = pack $ show params
                              putStrLn $ "Query params: " ++ (show params)
                              --l <- lift $ inc list
                              l <- inc list
                              --liftIO $ print' l
                              putStrLn $ show l
                              respond $ responseLBS status200 [("Content-Type", "text/plain")] respString

main :: IO ()
main = do putStrLn $ "Listening on port " ++ show 8080
          list <- newIORef []
          run 8080 (app list)

inc :: IORef [String] -> IO [String]
inc list = atomicModifyIORef list (\s -> ("lol":s, s))

--print' :: (Show a) => a -> IO ()
--print' x = do putStrLn $ "New state: " ++ show x

--mutate :: Eq a => String -> a -> [a] -> [a]
--mutate cmd elem s = case cmd of
--                        "add" -> (elem:s)
--                        "rem" -> filter (/=elem) s
--                        _     -> s
--mutate :: Eq a => Text -> a -> [a] -> ([a], Int)
--mutate cmd elem s = let result = case cmd of
--                                     "add" -> (elem:s)
--                                     "rem" -> filter (/=elem) s
--                                     _     -> s
--                    in (result, result)
