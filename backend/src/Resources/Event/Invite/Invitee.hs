{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Resources.Event.Invite.Invitee where
import           App                             (AppT, Config (dbPool))
import           Control.Monad.Reader            (asks)
import           Control.Monad.Trans             (liftIO)
import           Data.Aeson                      (FromJSON)
import           Data.Text                       (Text)
import           Database.Esqueleto.Experimental
import qualified Database.Models                 as DB
import qualified Database.Persist                as P
import           GHC.Generics                    (Generic)
import           Servant
import           Types                           (Email, EventId,
                                                  InviteId (InviteId),
                                                  Invitee (..),
                                                  InviteeId (InviteeId),
                                                  Status (..))
import           Utils                           (checkEventOrganizer, or404)


type InviteesAPI
  = GetInvitee
  :<|> GetInvitees
  :<|> PostInvitee
  :<|> PutInvitee
  :<|> DeleteInvitee

type GetInvitee = Capture "inviteeId" InviteeId :> Get '[JSON] Invitee
type GetInvitees = Get '[JSON] [Invitee]
type PostInvitee = ReqBody '[JSON] NewInvitee :> Post '[JSON] InviteeId
type PutInvitee = Capture "inviteeId" InviteeId :> ReqBody '[JSON] Invitee :> PutNoContent
type DeleteInvitee = Capture "inviteeId" InviteeId :> DeleteNoContent

newtype NewInvitee = NewInvitee {
  name :: Text
} deriving Generic

instance FromJSON NewInvitee

inviteesHandler :: Email -> EventId -> InviteId -> ServerT InviteesAPI (AppT IO)
inviteesHandler email eventId inviteId
  = getInvitee email eventId inviteId
  :<|> getInvitees email eventId inviteId
  :<|> postInvitee email eventId inviteId
  :<|> putInvitee email eventId inviteId
  :<|> deleteInvitee email eventId inviteId

getInvitee :: Email -> EventId -> InviteId -> InviteeId -> AppT IO Invitee
getInvitee email eventId _ (InviteeId inviteeId) = do
  checkEventOrganizer email eventId
  pool <- asks dbPool
  DB.Invitee{..} <- or404 . liftIO $ runSqlPool (get $ toSqlKey inviteeId) pool
  return Invitee {
    name = inviteeName
    , status = inviteeStatus
  }

getInvitees :: Email -> EventId -> InviteId -> AppT IO [Invitee]
getInvitees email eventId (InviteId inviteId)  = do
  checkEventOrganizer email eventId
  pool <- asks dbPool
  let query = select $ do
        event <- from $ table @DB.Invitee
        where_ $ event ^. DB.InviteeInvite  ==. val (toSqlKey @DB.Invite inviteId)
        return event
  liftIO $ (fmap . fmap) dbInviteeToApiInvitee (runSqlPool query pool)

dbInviteeToApiInvitee :: Entity DB.Invitee -> Invitee
dbInviteeToApiInvitee (Entity _ DB.Invitee{..}) =
  Invitee {
    name = inviteeName
    , status = inviteeStatus
  }

postInvitee :: Email -> EventId -> InviteId -> NewInvitee -> AppT IO InviteeId
postInvitee email eventId (InviteId inviteId) (NewInvitee inviteeName) = do
  checkEventOrganizer email eventId
  pool <- asks dbPool
  let newInvitee = DB.Invitee {
    DB.inviteeInvite = toSqlKey inviteId
    , DB.inviteeName = inviteeName
    , DB.inviteeStatus = Unknown
  }
  InviteeId . fromSqlKey <$> liftIO (runSqlPool (insert newInvitee) pool)


putInvitee :: Email -> EventId -> InviteId -> InviteeId -> Invitee -> AppT IO NoContent
putInvitee email eventId (InviteId inviteId) (InviteeId inviteeId) Invitee{..} = do
  checkEventOrganizer email eventId
  pool <- asks dbPool
  let newInvitee = DB.Invitee {
    DB.inviteeInvite = toSqlKey inviteId
    , DB.inviteeName = name
    , DB.inviteeStatus = status
    }
      query = replace (toSqlKey inviteeId) newInvitee
  liftIO $ runSqlPool query pool
  return NoContent


deleteInvitee :: Email -> EventId -> InviteId -> InviteeId -> AppT IO NoContent
deleteInvitee email eventId _ (InviteeId inviteeId) = do
  checkEventOrganizer email eventId
  pool <- asks dbPool
  let query = P.delete (toSqlKey @DB.Invitee inviteeId)
  liftIO $ runSqlPool query pool
  return NoContent
