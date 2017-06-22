{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Data.IORef (IORef, newIORef, atomicModifyIORef)
--import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.ByteString.Lazy.Char8
import Control.Monad (join)
import Control.Monad.Trans (liftIO, lift)

main :: IO ()
main = do putStrLn $ "Listening on port " ++ show 8080
          list <- newIORef []
          run 8080 (app list)

-- app :: IORef [a] -> (Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived)
app :: IORef [String] -> Application
app list request respond = do putStrLn $ logged request
                              let cmd = param request "command"
                                  elem = param request "element"
                              l <- inc cmd elem list 
                              putStrLn $ show l
                              respond $ responseLBS status200 [("Content-Type", "text/plain")] (pack $ show l)

-- unpack instead of show?
logged req = concat $ map show [requestMethod req,
                                rawPathInfo req,
                                rawQueryString req]

param req name = fmap unpack (join (lookup (pack name) (getParams req)))

inc :: Maybe String -> Maybe String -> IORef [String] -> IO [String]
inc cmd elem = modify f
               where f = chooseMod cmd elem 
                     modify f list = atomicModifyIORef list (\s -> (f s, f s))

chooseMod :: Maybe String -> Maybe String -> ([String] -> [String])
chooseMod (Just "add") (Just elem) = (\xs -> elem:xs)
chooseMod (Just "rem") (Just elem) = (\xs -> filter (/=elem) xs)
chooseMod _            _           = id

getParams req = map toString (queryString req)
                where toString (x, y) = (unpack x, fmap unpack y)
