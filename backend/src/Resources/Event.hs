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

type GetEvent = Capture "eventId" EventId :> Get '[JSON] Event
type GetEvents = Get '[JSON] [WithId Event]
type PostEvent = ReqBody '[JSON] Event :> Post '[JSON] EventId
type PutEvent = Capture "eventId" EventId :> ReqBody '[JSON] Event :> PutNoContent
type DeleteEvent = Capture "eventId" EventId :> Delete '[JSON] NoContent
type Invites = Capture "eventId" EventId :> "invites" :> InvitesAPI

type EventsAPI
  = GetEvent
  :<|> GetEvents
  :<|> PostEvent
  :<|> PutEvent
  :<|> DeleteEvent
  :<|> Invites

eventsHandler :: Email -> ServerT EventsAPI (AppT IO)
eventsHandler email
  = getEvent email
  :<|> getEvents email
  :<|> postEvent email
  :<|> putEvent email
  :<|> deleteEvent email
  :<|> invitesHandler email


getEvent :: Email -> EventId -> AppT IO Event
getEvent email eventId@(EventId eventId') = do
  checkEventOrganizer email eventId
  pool <- asks dbPool
  event <- or404 . liftIO $ runSqlPool (get $ toSqlKey eventId') pool
  return Event {
    name = DB.eventName event
    , description = DB.eventDescription event
    , startTime = DB.eventStartTime event
    , location = DB.eventLocation event
    }

getEvents :: Email -> AppT IO [WithId Event]
getEvents (Email email) = do
  pool <- asks dbPool
  let query = select $ do
        event <-
          from $
            table @DB.Event
        where_ $ event ^. DB.EventOrganizer  ==. val email
        return event
  result <- liftIO $ runSqlPool query pool
  return $ dbEventToApiEvent <$> result

postEvent :: Email -> Event -> AppT IO EventId
postEvent email event = do
  pool <- asks dbPool
  eventId <- liftIO $ runSqlPool (insert $ apiEventToDbEvent event email) pool
  return $ EventId $ fromSqlKey eventId

putEvent :: Email -> EventId -> Event -> AppT IO NoContent
putEvent email (EventId eventId) event = do
  checkEventOrganizer email (EventId eventId)
  pool <- asks dbPool
  let query = replace (toSqlKey eventId) (apiEventToDbEvent event email)
  liftIO $ runSqlPool query pool
  return NoContent

deleteEvent :: Email -> EventId -> AppT IO NoContent
deleteEvent email (EventId eventId) = do
  checkEventOrganizer email (EventId eventId)
  pool <- asks dbPool
  liftIO $ runSqlPool (P.delete (toSqlKey @DB.Event eventId)) pool
  return NoContent
  
dbEventToApiEvent :: Entity DB.Event -> WithId Event
dbEventToApiEvent (Entity id' event) =
  let event' = Event {
    name = DB.eventName event
    , description = DB.eventDescription event
    , startTime = DB.eventStartTime event
    , location = DB.eventLocation event
    }
  in WithId (fromSqlKey id') event'

apiEventToDbEvent :: Event -> Email -> DB.Event
apiEventToDbEvent Event{..} (Email email)= DB.Event {
  eventName = name
  , eventDescription = description
  , eventOrganizer = email
  , eventStartTime = startTime
  , eventLocation = location
  }
