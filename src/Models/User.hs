{-# LANGUAGE DeriveGeneric #-}

module Models.User where

import Butler.Schema
import Data.Text (Text)
import Data.Aeson (ToJSON(toJSON))
import GHC.Generics (Generic)

data User model m = User
    { userId :: Id model m Int
    , userName :: Text
    } deriving (Generic)