{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}

module Model where

import Butler.Schema
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson (ToJSON(toJSON))
import Butler.TypeInformation (TableSchema, ModelSelectors(schema))
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char

data User model m = User
    { userId :: Id model m Int
    , userName :: Text
    } deriving (Generic, KnitRecord Model)
deriving instance Show (User Model 'Resolved)
deriving instance Show (User Model 'Unresolved)
deriving instance Generic (Lazy Model User)
deriving instance ToJSON (Lazy Model User)
instance ToJSON (User Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 4 }

-- TODO: Posts to Tags is actually a many to many, as referenced by the array of foreign id's in each of them
-- We need to find a way so that when we run modelSelectors @(Model 'Unresolved), that it generates a bridge table
-- instead of saying that there's an id in each of the tables.
data Post model m = Post
    { postId :: Id model m Int
    , postTitle :: Text
    , postUser :: ForeignId model m "users" "userId"
    , postComments :: [ForeignId model m "comments" "commentId"]
    , postTags :: [ForeignId model m "tags" "tagId"]
    } deriving (Generic, KnitRecord Model)
deriving instance Show (Post Model 'Resolved)
deriving instance Show (Post Model 'Unresolved)
deriving instance Generic (Lazy Model Post)
deriving instance ToJSON (Lazy Model Post)
instance ToJSON (Post Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 4 }

data Tag model m = Tag
    { tagId :: Id model m Int
    , tagName :: Text
    , tagPosts :: [ForeignId model m "posts" "postId"]
    } deriving (Generic, KnitRecord Model)
deriving instance Show (Tag Model 'Resolved)
deriving instance Show (Tag Model 'Unresolved)
deriving instance Generic (Lazy Model Tag)
deriving instance ToJSON (Lazy Model Tag)
instance ToJSON (Tag Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 3 }

data Comment model m = Comment
    { commentId :: Id model m Int
    , commentText :: Text
    , commentUser :: ForeignId model m "users" "userId"
    , commentPost :: ForeignId model m "posts" "postId"
    } deriving (Generic, KnitRecord Model)
deriving instance Show (Comment Model 'Unresolved)
deriving instance Show (Comment Model 'Resolved)
deriving instance Generic (Lazy Model Comment)
deriving instance ToJSON (Lazy Model Comment)
instance ToJSON (Comment Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 7 }

data Model m = Model
    { users :: Table Model m User
    , posts :: Table Model m Post
    , comments :: Table Model m Comment
    , tags :: Table Model m Tag
    } deriving (Generic, KnitTables)
deriving instance Show (Model 'Resolved)
deriving instance Show (Model 'Unresolved)

currentModelMigration :: TableSchema
currentModelMigration = schema @Model

{-
data User model m = User
    { userId :: Id model m Int
    , userName :: Text
    , userPosts :: [ForeignId model m "posts" "postId"]
    , userComments :: [ForeignId model m "comments" "commentId"]
    } deriving (Generic, KnitRecord Model)

deriving instance Show (User Model 'Unresolved)
deriving instance Show (User Model 'Resolved)
deriving instance Generic (Lazy Model User)
deriving instance ToJSON (Lazy Model User)
instance ToJSON (User Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 4 }

data Post model m = Post
    { postId :: Id model m Int
    , postTitle :: Text
    , postUser :: ForeignId model m "users" "userId"
    , postComments :: [ForeignId model m "comments" "commentId"]
    } deriving (Generic, KnitRecord Model)

deriving instance Generic (Lazy Model Post)
deriving instance ToJSON (Lazy Model Post)
deriving instance Show (Post Model 'Unresolved)
deriving instance Show (Post Model 'Resolved)
instance ToJSON (Post Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 4 }

data Comment model m = Comment
    { commentId :: Id model m Int
    , commentText :: Text
    , commentUser :: ForeignId model m "users" "userId"
    , commentPost :: ForeignId model m "posts" "postId"
    } deriving (Generic, KnitRecord Model)

deriving instance Generic (Lazy Model Comment)
deriving instance ToJSON (Lazy Model Comment)
deriving instance Show (Comment Model 'Unresolved)
deriving instance Show (Comment Model 'Resolved)
instance ToJSON (Comment Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 7 }

data A model m = A
    { aId :: Id model m Int
    , aName :: Text
    , aBs :: [ForeignId model m "bs" "bId"]
    } deriving (Generic, KnitRecord Model)

deriving instance Generic (Lazy Model A)
deriving instance ToJSON (Lazy Model A)
deriving instance Show (A Model 'Unresolved)
deriving instance Show (A Model 'Resolved)
instance ToJSON (A Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 1 }

data B model m = B
    { bId :: Id model m Int 
    , bName :: Text
    , bAs :: [ForeignId model m "as" "aId"]
    } deriving (Generic, KnitRecord Model)

deriving instance Generic (Lazy Model B)
deriving instance ToJSON (Lazy Model B)
deriving instance Show (B Model 'Unresolved)
deriving instance Show (B Model 'Resolved)
instance ToJSON (B Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 1 }

data Model m = Model
    { posts :: Table Model m Post
    , users :: Table Model m User
    , comments :: Table Model m Comment
    , as :: Table Model m A
    , bs :: Table Model m B
    } deriving (Generic, KnitTables)

deriving instance Show (Model 'Resolved)

user :: User Model 'Unresolved
user = User
    { userId = Id 1
    , userName = "Foo"
    , userPosts = []
    , userComments = []
    }
post :: Post Model 'Unresolved
post = Post
    { postId = Id 1
    , postTitle = "This is a test"
    , postComments = [ForeignId 1, ForeignId 2]
    , postUser = ForeignId 1
    }
comment :: Comment Model 'Unresolved
comment = Comment
    { commentId = Id 1
    , commentText = "One more time"
    , commentUser = ForeignId 1
    , commentPost = ForeignId 1
    }
comment2 :: Comment Model 'Unresolved
comment2 = Comment
    { commentId = Id 2
    , commentText = "One more time"
    , commentUser = ForeignId 1
    , commentPost = ForeignId 1
    }
model :: Model 'Unresolved
model = Model
    { posts = [post]
    , users = [user]
    , comments = [comment, comment2]
    , as = []
    , bs = []
    }
test :: Model 'Resolved
test = case knit model of
    Right resolved -> resolved
    Left _ -> error "Nope"

-}