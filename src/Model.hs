{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Model where

import Butler.Schema
import Butler.TypeInformation (DatabaseSchema, ModelSelectors(schema))
import Data.Aeson (ToJSON(toJSON))
import Data.Text (Text)
import GHC.Generics (Generic)
import Models.Comment
import Models.Post
import Models.Tag
import Models.User
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char

type User' = User Model 'Resolved
deriving instance KnitRecord Model User
deriving instance Show (User Model 'Resolved)
deriving instance Show (User Model 'Unresolved)
deriving instance Generic (Lazy Model User)
deriving instance ToJSON (Lazy Model User)
instance ToJSON (User Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 4 }

type Post' = Post Model 'Resolved
deriving instance KnitRecord Model Post
deriving instance Show (Post Model 'Resolved)
deriving instance Show (Post Model 'Unresolved)
deriving instance Generic (Lazy Model Post)
deriving instance ToJSON (Lazy Model Post)
instance ToJSON (Post Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 4 }

type Tag' = Tag Model 'Resolved
deriving instance KnitRecord Model Tag
deriving instance Show (Tag Model 'Resolved)
deriving instance Show (Tag Model 'Unresolved)
deriving instance Generic (Lazy Model Tag)
deriving instance ToJSON (Lazy Model Tag)
instance ToJSON (Tag Model 'Resolved) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = fmap Char.toLower . Prelude.drop 3 }

type Comment' = Comment Model 'Resolved
deriving instance KnitRecord Model Comment
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

currentModelMigration :: DatabaseSchema
currentModelMigration = schema @Model