{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module User.User where

import Data.Aeson
import Butler.Schema
import Data.Text (Text)
import GHC.Generics (Generic)

data User = User { userId :: Int, userEmail :: Text } deriving (Eq, Show, Generic, ToJSON, Entity)