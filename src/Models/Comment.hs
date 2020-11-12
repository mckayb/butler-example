{-# LANGUAGE DeriveGeneric #-}

module Models.Comment where

import Butler.Schema
import Data.Text (Text)
import GHC.Generics (Generic)

data Comment model m = Comment
    { commentId :: Id model m Int
    , commentText :: Text
    , commentUser :: ForeignId model m "users" "userId"
    , commentPost :: ForeignId model m "posts" "postId"
    } deriving (Generic)