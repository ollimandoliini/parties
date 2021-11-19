{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Invites where

import Data.Aeson.TH
import Servant

import App (AppT)

data EnrollmentPayload = EnrollmentPayload {
    partyId :: String
    , participantId :: String
    , attending :: Bool
} 

type EnrollmentAPI = "enroll" :> ReqBody '[JSON] EnrollmentPayload :> PostNoContent

type InvitesAPI = "invites" :> Capture "inviteId" String :> Get '[JSON] (Maybe Invite)

data Party = Party {
    partyId :: String
    , partyName :: String
    , description :: String
}

data Participant = Participant {
    participantId :: String
    , name :: String
}

data Invite = Invite {
    party' :: Party
  , participants :: [Participant]
}

$(deriveJSON defaultOptions ''Party)
$(deriveJSON defaultOptions ''Participant)
$(deriveJSON defaultOptions ''Invite)
$(deriveJSON defaultOptions ''EnrollmentPayload)


party :: Party
party = Party "a" "Ollin synttärit" "Töttöröö"

invites :: [(String, Invite)]
invites = [
  ("a", Invite party [
      Participant "a" "Riku"
      , Participant "b" "Malla"
    ])
  , ("b", Invite party [
      Participant "c" "Markus",
      Participant "d" "Ella"])
  ]



getInviteHandler :: String -> AppT IO (Maybe Invite)
getInviteHandler id' =
  return $ lookup id' invites

enrollmentHandler :: EnrollmentPayload -> AppT IO NoContent 
enrollmentHandler (EnrollmentPayload partyId participantId attending) = do
    return NoContent


inviteHandler :: ServerT InvitesAPI (AppT IO)
inviteHandler = getInviteHandler