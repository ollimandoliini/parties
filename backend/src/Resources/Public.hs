{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RecordWildCards #-}
module Resources.Public where
import           App                             (AppT, Config (..))
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Reader            (asks)
import qualified Database                        as DB
import           Database.Esqueleto.Experimental
import           Servant
import           Types                           (EventId (..), InviteCode (..), Event (..), WithId (WithId), Invitee (Invitee), EventInvite (..), InviteeId (InviteeId), StatusPayload (StatusPayload))
import           Utils                           (or404)

type GetEventInvite =
    "event"
        :> Capture "eventId" EventId
        :> "invite-code"
        :> Capture "inviteCode" InviteCode
        :> Get '[JSON] EventInvite

type ChangeInviteStatus =
    "event"
        :> Capture "eventId" EventId
        :> "invite-code"
        :> Capture "inviteCode" InviteCode
        :> "invitee"
        :> Capture "inviteeId" InviteeId
        :> ReqBody '[JSON] StatusPayload
        :> Patch '[JSON] NoContent

type PublicAPI
    = GetEventInvite
    :<|> ChangeInviteStatus

publicApiHandler :: ServerT PublicAPI (AppT IO)
publicApiHandler = getEventInvite :<|> changeInviteeStatus

getEventInvite :: EventId -> InviteCode -> AppT IO EventInvite
getEventInvite (EventId eventId) (InviteCode inviteCode) = do
    pool <- asks dbPool
    let inviteQuery = selectOne $ do
                invite <- from $ table @DB.Invite
                where_ $ ((invite ^. DB.InviteCode) ==. val inviteCode) &&. ((invite ^. DB.InviteEvent) ==. val (toSqlKey eventId))
                return invite
    (Entity inviteId _) <- or404 . liftIO $ runSqlPool inviteQuery pool
    let eventQuery = selectOne $ do
            event <- from $ table @DB.Event
            where_ ((event ^. DB.EventId) ==. val (toSqlKey eventId))
            return event
    (Entity _ DB.Event{..}) <- or404 . liftIO $ runSqlPool eventQuery pool
    let inviteesQuery = select $ do
            invitee <- from $ table @DB.Invitee
            where_ ((invitee ^. DB.InviteeInvite) ==. val inviteId)
            return invitee
    invitees <- liftIO $ runSqlPool inviteesQuery pool
    return $ EventInvite {
        eventInfo = Event {
            name = eventName
            , description = eventDescription
            , location = eventLocation
            , startTime = eventStartTime
        }
        , invitees = dbInviteeToApiInvitee <$> invitees
    }
    where
        dbInviteeToApiInvitee (Entity inviteeId DB.Invitee{..}) = WithId (fromSqlKey inviteeId) (Invitee inviteeName inviteeStatus)

changeInviteeStatus :: EventId -> InviteCode -> InviteeId -> StatusPayload -> AppT IO NoContent
changeInviteeStatus (EventId eventId) (InviteCode inviteCode) (InviteeId inviteeId) (StatusPayload status) = do
    pool <- asks dbPool
    let inviteQuery = selectOne $ do
            invite <- from $ table @DB.Invite
            where_ $ ((invite ^. DB.InviteCode) ==. val inviteCode) &&. ((invite ^. DB.InviteEvent) ==. val (toSqlKey eventId))
            return invite
    _ <- or404 . liftIO $ runSqlPool inviteQuery pool
    let updateQuery = update $ \p -> do
                        set p [ DB.InviteeStatus =. val status]
                        where_ $ (p ^. DB.InviteeId) ==. val (toSqlKey inviteeId)
                        return ()
    liftIO $ runSqlPool updateQuery pool
    return NoContent

