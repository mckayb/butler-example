{-# LANGUAGE TypeOperators #-}

module Controllers.UserController where

import Butler.Schema
import Butler.Routing
import Model
import Models.User

type UserRoutes = "users" :> Get '[JSON] [User']

indexUsers :: [User']
indexUsers = []