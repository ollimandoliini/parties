{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Aeson (FromJSON (parseJSON), ToJSON(..), object, (.=), genericParseJSON, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TypeScript.TH (HasJSONOptions (getJSONOptions))
import Data.Aeson.TH (defaultOptions)
import Data.Time (UTCTime)
import Database.Persist.TH (derivePersistField)
import Servant (FromHttpApiData)

data WithId a = WithId Int a
  deriving (Show, Eq)

instance ToJSON a => ToJSON (WithId a) where
  toJSON (WithId id' a) = object ["id" .= id', "data" .= toJSON a]

data Event = Event
  { name :: Text
    , description :: Text
    , startTime :: UTCTime
    , location :: Text
  }
  deriving (Show, Eq, Generic)


instance FromJSON Event
instance ToJSON Event

instance HasJSONOptions Event where
    getJSONOptions _ = defaultOptions

newtype Email = Email Text

newtype EventId = EventId
  { id :: Int}
  deriving (Generic, FromHttpApiData)

instance FromJSON EventId
instance ToJSON EventId

data Invite = Invite {
  code :: Text
  , invitees :: [Invitee]
} deriving (Show, Eq, Generic)

newtype InviteId = InviteId {
  id :: Int
  } deriving (Generic, FromHttpApiData)

instance FromJSON InviteId
instance ToJSON InviteId

data Invitee = Invitee {
  name :: Text
  , status :: Status
} deriving (Show, Eq, Generic)

newtype InviteeId = InviteeId {
   id :: Int
  } deriving (Generic, FromHttpApiData)

instance FromJSON InviteeId
instance ToJSON InviteeId




data Status
  = Accepted
  | Tentative
  | Declined
  | Unknown
  deriving (Show, Eq, Read, Generic)

derivePersistField "Status"

instance ToJSON Status where
  toJSON = genericToJSON defaultOptions 

instance FromJSON Status where
  parseJSON = genericParseJSON defaultOptions 



instance FromJSON Invitee
instance ToJSON Invitee


instance ToJSON Invite

instance HasJSONOptions Invite where
    getJSONOptions _ = defaultOptions
