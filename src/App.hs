module App (start) where

import Butler.Routing
import Routes
import Network.Wai
import Network.Wai.Handler.Warp

start :: IO ()
start = do
    putStrLn "Running on http://localhost:8080"
    run 8080 app

app :: Application
app = serve api server
