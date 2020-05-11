{-# LANGUAGE TypeOperators #-}

module Routes where

import User.Controller
import Butler.Routing
import Model
import Butler.Schema

type Routes = "users" :> Get '[JSON] [User Model 'Resolved]

api :: Proxy Routes
api = Proxy

server :: Server Routes
server = return indexUsers
