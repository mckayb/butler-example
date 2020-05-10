{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where

import Butler.Schema
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson (ToJSON(toJSON))
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char

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