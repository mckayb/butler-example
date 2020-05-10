module User.Controller where

import Butler.Schema
import Model

indexUsers :: [User Model 'Resolved]
indexUsers = [User 1 "foo" [] []]