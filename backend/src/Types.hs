{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson (FromJSON (..), ToJSON(..), object, (.=), Value (Object, Null))
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TypeScript.TH (HasJSONOptions (getJSONOptions))
import Data.Aeson.TH (defaultOptions)
import Data.Time (UTCTime)
import Database.Persist.TH (derivePersistField)
import Servant (FromHttpApiData)
import Data.Int (Int64)
import Data.HashMap.Strict (insert)

data WithId a = WithId Int64 a
  deriving (Show, Eq)

instance ToJSON a => ToJSON (WithId a) where
  toJSON (WithId id' a) = case toJSON a of
    Object hm -> Object (insert "id" (toJSON id') hm)
    Null -> Null
    other -> object [
      "id" .= id'
      , "data" .= toJSON other
      ]

data Event = Event
  { name :: Text
    , description :: Text
    , startTime :: UTCTime
    , location :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


instance HasJSONOptions Event where
    getJSONOptions _ = defaultOptions

newtype Email = Email Text

newtype InviteCode = InviteCode Text
  deriving newtype FromHttpApiData

newtype EventId = EventId
  { id :: Int64 }
  deriving stock Generic
  deriving newtype FromHttpApiData
  deriving anyclass (FromJSON, ToJSON)


data Invite = Invite {
  code :: Text
  , invitees :: [Invitee]
} deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype InviteId = InviteId
  { id :: Int64 }
  deriving stock Generic
  deriving newtype FromHttpApiData
  deriving anyclass (FromJSON, ToJSON)


data Invitee = Invitee {
  name :: Text
  , status :: Status
} deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype InviteeId = InviteeId {
   id :: Int64
  } deriving stock Generic
    deriving newtype FromHttpApiData
    deriving anyclass (FromJSON, ToJSON)


data Status
  = Accepted
  | Tentative
  | Declined
  | Unknown
  deriving stock (Show, Eq, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

derivePersistField "Status"

instance HasJSONOptions Invite where
    getJSONOptions _ = defaultOptions

instance HasJSONOptions Invitee where
    getJSONOptions _ = defaultOptions

instance HasJSONOptions Status where
    getJSONOptions _ = defaultOptions


-- PUBLIC

data EventInvite = EventInvite {
    eventInfo :: Event
    , invitees :: [WithId Invitee]
} deriving stock Generic
  deriving anyclass (ToJSON)

newtype StatusPayload = StatusPayload {
    status :: Status
} deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

instance HasJSONOptions (WithId a) where
    getJSONOptions _ = defaultOptions

instance HasJSONOptions EventInvite where
    getJSONOptions _ = defaultOptions
