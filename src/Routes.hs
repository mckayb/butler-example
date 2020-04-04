{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Data.Text (Text)
import Todo.Todo
import User.User
import User.Controller
import Butler.Routing

type Routes = "todo" :> Get '[JSON] [User]

api :: Proxy Routes
api = Proxy

server :: Server Routes
server = return $ indexUsers
