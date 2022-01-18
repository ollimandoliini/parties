{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types where

import           Data.Aeson               (FromJSON (..), ToJSON (..),
                                           Value (Null, Object), object, (.=))
import           Data.Aeson.TH            (defaultOptions)
import           Data.Aeson.TypeScript.TH (HasJSONOptions (getJSONOptions))
import           Data.HashMap.Strict      (insert)
import           Data.Int                 (Int64)
import           Data.Text                (Text)
import           Data.Time                (UTCTime)
import           Database.Persist.TH      (derivePersistField)
import           GHC.Generics             (Generic)
import           Servant                  (FromHttpApiData)

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
  { name               :: Text
    , description      :: Text
    , startTime        :: UTCTime
    , location         :: Text
    , invitesArePublic :: Bool
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
  code       :: Text
  , invitees :: [Invitee]
} deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype InviteId = InviteId
  { id :: Int64 }
  deriving stock Generic
  deriving newtype FromHttpApiData
  deriving anyclass (FromJSON, ToJSON)


data Invitee = Invitee {
  name     :: Text
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
    eventInfo  :: Event
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
