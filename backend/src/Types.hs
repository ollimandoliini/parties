{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Event = Event
  { name :: Text,
    description :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Event
instance ToJSON Event

type Email = Text

newtype EventId = EventId
  { id :: Int
  }
  deriving (Generic)

instance FromJSON EventId
instance ToJSON EventId


data Invite = Invite {
  code :: Text
  , invitees :: [Text]
} deriving (Show, Eq, Generic)

instance ToJSON Invite
