{-# LANGUAGE TypeOperators #-}

module Routes where

import Controllers.UserController
import Butler.Routing
import Model
import Butler.Schema
import Models.User

type Routes = "users" :> Get '[JSON] [User Model 'Resolved]

api :: Proxy Routes
api = Proxy

server :: Server Routes
server = return indexUsers
