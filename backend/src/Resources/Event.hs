{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}

module Resources.Event where

import App
import Auth
import Control.Monad ( forM_, guard, replicateM)
import Control.Monad.Reader (MonadIO (liftIO), ask, asks)
import Data.Aeson ( FromJSON, ToJSON )
import Data.Map ( fromListWith, toList )
import Data.Text ( Text, pack )
import Data.Void
import qualified Database (Event (..))
import qualified Database as DB
import Database.Esqueleto.Experimental
import qualified Database.Persist as P
import Database.Persist.Sql (fromSqlKey, runSqlPool, toSqlKey)
import GHC.Generics hiding (from)
import System.Random ( randomRIO )
import Servant
import Types




type PartiesAPI =
  "events" :> (ReqBody '[JSON] Event :> Post '[JSON] EventId
    :<|> 
      Get '[JSON] [Event]
    :<|> 
      (
        Capture "eventId" Int :> 
          (
            Get '[JSON] Event
            :<|> Delete '[JSON] ()
            :<|> "invites" :> InvitesAPI
        )
      )
    )
  

eventsHandler email =
  createEventHandler email
    :<|> getUserEvents email
    :<|> (\int -> getEvent email int :<|> deleteEventHandler email int:<|> invitesHandler email int)

or404 :: AppT IO (Maybe b) -> AppT IO b
or404 res = res >>= maybe (throwError err404) pure

getEvent :: Email -> Int -> AppT IO Event
getEvent email eventId = do
  pool <- asks dbPool
  checkEventOrganizer email eventId
  event <- or404 . liftIO $ runSqlPool (get $ toSqlKey $ fromIntegral eventId) pool
  return $ Event (DB.eventName event) (DB.eventDescription event)

getUserEvents :: Email -> AppT IO [Event]
getUserEvents email = do
  pool <- asks dbPool
  let query = select $ do
        event <-
          from $
            table @DB.Event
        where_ $ event ^. DB.EventOrganizer  ==. val email
        return event
  result <- liftIO $ runSqlPool query pool
  return $ dbEventToApiEvent <$> result

dbEventToApiEvent :: Entity DB.Event -> Event
dbEventToApiEvent (Entity _ event) = Event (DB.eventName event) (DB.eventDescription event)

createEventHandler :: Email -> Event -> AppT IO EventId
createEventHandler email party = do
  pool <- asks dbPool
  let newEvent = Database.Event (name party) email (description party)
  partyId <- liftIO $ runSqlPool (insert newEvent) pool
  return $ EventId $ fromIntegral $ fromSqlKey partyId

deleteEventHandler :: Email -> Int -> AppT IO ()
deleteEventHandler email eventId = do
  pool <- asks dbPool
  checkEventOrganizer email eventId
  liftIO $ runSqlPool (P.delete eventIdKey) pool
  where
    eventIdKey :: Key DB.Event
    eventIdKey = toSqlKey $ fromIntegral eventId

checkEventOrganizer :: Text -> Int -> AppT IO ()
checkEventOrganizer email eventId = do
  pool <- asks dbPool
  DB.Event{ eventOrganizer } <- or404 . liftIO $ runSqlPool (get eventIdKey) pool
  if eventOrganizer == email
    then return ()
    else throwError err401
  where
    eventIdKey :: Key DB.Event
    eventIdKey = toSqlKey $ fromIntegral eventId



type InvitesAPI = Get '[JSON] [Invite] 
                  :<|> ReqBody '[JSON] [Text] :> Post '[JSON] Int
 
-- invitesHandler :: Int -> Email -> (Int -> AppT IO [Invite]) :<|> (Email -> AppT IO (Int, Invite))
-- invitesHandler :: Email -> Int -> AppT IO [Invite]
invitesHandler email int = getEventInvites email int :<|> postEventInvite email int

getEventInvites :: Email -> Int -> AppT IO [Invite]
getEventInvites email eventId = do
  pool <- asks dbPool
  checkEventOrganizer email eventId
  let inviteQuery = select $ do
              event :& invite :& invitee <- from $ table @DB.Event
                `innerJoin` table @DB.Invite
                `on` (\(event :& invite) -> (event ^. DB.EventId) ==. (invite ^. DB.InviteEvent))
                `innerJoin` table @DB.Invitee
                `on` (\(event :& invite :& invitees) -> (invite ^. DB.InviteId) ==. (invitees ^. DB.InviteeInvite))
              where_ $ event ^. DB.EventId ==. val (toSqlKey $ fromIntegral eventId)
              return (invite :& invitee)
  invites <- liftIO $ runSqlPool inviteQuery pool
  return $ groupInvites invites
  where
    groupInvites invites = toInvite <$> toList (fromListWith (++) [(inviteCode, [invitee]) | (Entity id DB.Invite{..} :& Entity _ invitee) <- invites])
    toInvite :: (Text, [DB.Invitee]) -> Invite
    toInvite (inviteCode, invitees') = Invite {
      code = inviteCode
      , invitees = DB.inviteeName <$> invitees'
    }

postEventInvite :: Email -> Int -> [Text] -> AppT IO Int
postEventInvite email eventId invitees = do
  checkEventOrganizer email eventId
  pool <- asks dbPool
  inviteCode <- generateNewInviteCode

  let newInvite = DB.Invite {
    inviteEvent = toSqlKey $ fromIntegral eventId
    , inviteCode = inviteCode
    }
  inviteId <- liftIO $ runSqlPool (insert newInvite) pool
  liftIO $ mapM (\invitee -> runSqlPool (insert (DB.Invitee inviteId invitee)) pool) invitees
  return $ fromIntegral $ fromSqlKey inviteId

generateNewInviteCode :: AppT IO Text
generateNewInviteCode = do
  existingInviteCodes <- getExistingInviteCodes
  liftIO $ loop existingInviteCodes
  where
    loop existingInviteCodes = do
      inviteCode <- generateInviteCode
      if inviteCode `elem` existingInviteCodes
        then loop existingInviteCodes
        else return inviteCode



getExistingInviteCodes :: AppT IO [Text]
getExistingInviteCodes = do
  pool <- asks dbPool
  let query = select $ distinct $ do 
        invites <- from $ table @DB.Invite
        return $ invites ^. DB.InviteCode
  (fmap . fmap) unValue (liftIO $ runSqlPool query pool)


generateInviteCode :: IO Text
generateInviteCode = 
  pack <$> replicateM 6 (randomItemFromList chars)
  where
    chars = ['0'..'9'] ++ ['a'..'z'] ++ ['A' .. 'Z'] 

randomItemFromList :: MonadIO m => [b] -> m b
randomItemFromList li = do
  i <- randomRIO (0, length li - 1)
  return (li !! i)


        
  

