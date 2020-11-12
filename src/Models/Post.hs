{-# LANGUAGE DeriveGeneric #-}

module Models.Post where

import Butler.Schema
import Data.Text (Text)
import GHC.Generics (Generic)

data Post model m = Post
    { postId :: Id model m Int
    , postTitle :: Text
    , postUser :: ForeignId model m "users" "userId"
    , postComments :: [ForeignId model m "comments" "commentId"]
    , postTags :: [ForeignId model m "tags" "tagId"]
    } deriving (Generic)