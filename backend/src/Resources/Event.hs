{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Resources.Event where

import           App
import           Control.Monad.Reader            (MonadIO (liftIO), asks)
import qualified Database                        as DB
import           Database.Esqueleto.Experimental
import qualified Database.Persist                as P
import           Servant
import           Types
import Resources.Event.Invite (InvitesAPI, invitesHandler)
import Utils (checkEventOrganizer, or404)


-- EVENTS

type PostEvent = ReqBody '[JSON] Event :> Post '[JSON] EventId
type GetUserEvents = Get '[JSON] [WithId Event]
type GetEvent = Capture "eventId" EventId :> Get '[JSON] Event
type DeleteEvent = Capture "eventId" EventId :> Delete '[JSON] NoContent
type Invites = Capture "eventId" EventId :> "invites" :> InvitesAPI


type EventsAPI
  = PostEvent
  :<|> GetUserEvents
  :<|> GetEvent
  :<|> DeleteEvent
  :<|> Invites


eventsHandler :: Email -> ServerT EventsAPI (AppT IO)
eventsHandler email
  = createEventHandler email
  :<|> getUserEvents email
  :<|> getEvent email
  :<|> deleteEventHandler email
  :<|> invitesHandler email


getEvent :: Email -> EventId -> AppT IO Event
getEvent email (EventId eventId) = do
  checkEventOrganizer email (EventId eventId)
  pool <- asks dbPool
  event <- or404 . liftIO $ runSqlPool (get $ toSqlKey $ fromIntegral eventId) pool
  return Event {
    name = DB.eventName event
    , description = DB.eventDescription event
    , startTime = DB.eventStartTime event
    , location = DB.eventLocation event
    }

getUserEvents :: Email -> AppT IO [WithId Event]
getUserEvents (Email email) = do
  pool <- asks dbPool
  let query = select $ do
        event <-
          from $
            table @DB.Event
        where_ $ event ^. DB.EventOrganizer  ==. val email
        return event
  result <- liftIO $ runSqlPool query pool
  return $ dbEventToApiEvent <$> result

dbEventToApiEvent :: Entity DB.Event -> WithId Event
dbEventToApiEvent (Entity id' event) =
  let event' = Event {
    name = DB.eventName event
    , description = DB.eventDescription event
    , startTime = DB.eventStartTime event
    , location = DB.eventLocation event
    }
  in WithId (fromIntegral $ fromSqlKey id') event'

apiEventToDbEvent :: Event -> Email -> DB.Event
apiEventToDbEvent Event{..} (Email email) = DB.Event {
  eventName = name
  , eventDescription = description
  , eventOrganizer = email
  , eventStartTime = startTime
  , eventLocation = location
}


createEventHandler :: Email -> Event -> AppT IO EventId
createEventHandler email event = do
  pool <- asks dbPool
  eventId <- liftIO $ runSqlPool (insert $ apiEventToDbEvent event email) pool
  return $ EventId $ fromIntegral $ fromSqlKey eventId

deleteEventHandler :: Email -> EventId -> AppT IO NoContent
deleteEventHandler email (EventId eventId) = do
  checkEventOrganizer email (EventId eventId)
  pool <- asks dbPool
  liftIO $ runSqlPool (P.delete eventIdKey) pool
  return NoContent
  where
    eventIdKey :: Key DB.Event
    eventIdKey = toSqlKey $ fromIntegral eventId

