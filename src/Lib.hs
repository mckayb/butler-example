module Lib
    ( someFunc
    ) where

import Routes
import Servant
import Network.Wai
import Network.Wai.Handler.Warp

someFunc :: IO ()
someFunc = run 8080 app

-- server = run 8080 . serve

app :: Application
app = serve api server
