{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Todo.Todo where

import Data.Text (Text)
import Butler.Schema (Entity)
import GHC.Generics (Generic)

data Todo = Todo { todoContent :: Text } deriving (Generic, Show, Eq, Entity)
