{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Development (startDevelopmentServer) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.WebSocket
import qualified Network.WebSockets as WS
import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

type API = "ws" :> WebSocket

wsApp :: WS.Connection -> Handler ()
wsApp conn = liftIO $ do
    let srcDir = "src"  
    
    _ <- withManager $ \mgr -> do
        _ <- watchTree mgr srcDir (const True) $ \event -> do
            putStrLn $ "File changed: " ++ show event
            WS.sendTextData conn ("reload" :: Text)
        
        forever $ threadDelay 1000000
            
    _ <- WS.receive conn
    pure ()

server :: Server API
server = wsApp

app :: Application
app = serve (Proxy :: Proxy API) server

startDevelopmentServer :: IO ()
startDevelopmentServer = do
    putStrLn "WebSocket server starting on port 10000..."
    run 10000 app

