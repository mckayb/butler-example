{-# LANGUAGE DeriveGeneric #-}

module Models.Tag where

import Butler.Schema
import Data.Text (Text)
import GHC.Generics (Generic)

data Tag model m = Tag
    { tagId :: Id model m Int
    , tagName :: Text
    , tagPosts :: [ForeignId model m "posts" "postId"]
    } deriving (Generic)