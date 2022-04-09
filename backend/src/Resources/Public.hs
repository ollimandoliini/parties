{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Resources.Public where
import           App                             (AppT, Config (..))
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Reader            (asks)
import           Database.Esqueleto.Experimental
import qualified Database.Models                 as DB
import           Servant
import           Types                           (Event (..), EventId (..),
                                                  EventInvite (..),
                                                  InviteCode (..),
                                                  Invitee (..),
                                                  InviteeId (InviteeId),
                                                  StatusPayload (StatusPayload),
                                                  WithId (WithId), status)
import           Utils                           (or404)
import Servant.Auth.Server (throwAll)

type GetEventInvite =
    "event"
        :> Capture "eventId" EventId
        :> "invite-code"
        :> Capture "inviteCode" InviteCode
        :> Get '[JSON] EventInvite

type GetOtherInvitees =
    "event"
        :> Capture "eventId" EventId
        :> "invite-code"
        :> Capture "inviteCode" InviteCode
        :> "invitees"
        :> Get '[JSON] [WithId Invitee]

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
    :<|> GetOtherInvitees
    :<|> ChangeInviteStatus

publicApiHandler :: ServerT PublicAPI (AppT IO)
publicApiHandler
    = getEventInvite
    :<|> getOtherInvitees
    :<|> changeInviteeStatus

getEventInvite :: EventId -> InviteCode -> AppT IO EventInvite
getEventInvite (EventId eventId) (InviteCode inviteCode) = do
    pool <- asks dbPool
    (Entity inviteId _) <- getInvite (EventId eventId) (InviteCode inviteCode)
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
            , invitesArePublic = eventInvitesArePublic
        }
        , invitees = dbInviteeToApiInvitee <$> invitees
    }
    where
        dbInviteeToApiInvitee (Entity inviteeId DB.Invitee{..}) = WithId (fromSqlKey inviteeId) (Invitee inviteeName inviteeStatus)

getInvite :: EventId -> InviteCode -> AppT IO (Entity DB.Invite)
getInvite (EventId eventId) (InviteCode inviteCode) = do
    pool <- asks dbPool
    let inviteQuery = selectOne $ do
                invite <- from $ table @DB.Invite
                where_ $ ((invite ^. DB.InviteCode) ==. val inviteCode) &&. ((invite ^. DB.InviteEvent) ==. val (toSqlKey eventId))
                return invite
    or404 . liftIO $ runSqlPool inviteQuery pool

getOtherInvitees :: EventId -> InviteCode -> AppT IO [WithId Invitee]
getOtherInvitees (EventId eventId) (InviteCode inviteCode) = do
    pool <- asks dbPool
    _ <- getInvite (EventId eventId) (InviteCode inviteCode)
    let eventQuery = selectOne $ do
                event <- from $ table @DB.Event
                where_ ((event ^. DB.EventId) ==. val (toSqlKey eventId))
                return event
    (Entity _ event) <- or404 . liftIO $ runSqlPool eventQuery pool
    if DB.eventInvitesArePublic event
        then 
            fmap dbInviteeToApiInvitee <$> getInviteesForEvent (EventId eventId)
        else
            throwAll err403
    where
        dbInviteeToApiInvitee (Entity id' DB.Invitee{..}) = WithId (fromSqlKey id') (Invitee { name = inviteeName, status = inviteeStatus})


    

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

getInviteesForEvent :: EventId -> AppT IO [Entity DB.Invitee]
getInviteesForEvent (EventId eventId) = do
    pool <- asks dbPool
    let inviteQuery =
          select $ do
            event :& _ :& invitee <- from $ table @DB.Event
              `innerJoin` table @DB.Invite
              `on` (\(event :& invite) -> event ^. DB.EventId ==. invite ^. DB.InviteEvent)
              `innerJoin` table @DB.Invitee
              `on` (\(_ :& invite :& invitees) -> invite ^. DB.InviteId ==. (invitees ^. DB.InviteeInvite))
            where_ $ event ^. DB.EventId ==. val (toSqlKey eventId)
            return invitee
    liftIO $ runSqlPool inviteQuery pool
