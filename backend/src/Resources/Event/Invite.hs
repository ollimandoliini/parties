{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Resources.Event.Invite where
import           App                             (AppT, Config (dbPool))
import           Control.Monad                   (replicateM)
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Reader            (asks)
import           Data.Map                        (fromListWith, toList)
import           Data.Text                       (Text, pack)
import qualified Database                        as DB
import           Database.Esqueleto.Experimental
import           Resources.Event.Invite.Invitee  (InviteesAPI, inviteesHandler)
import           Servant
import           System.Random                   (randomRIO)
import           Types                           (Email, EventId (EventId),
                                                  Invite (..), InviteId (InviteId),
                                                  Invitee (..))
import           Utils                           (checkEventOrganizer, checked)
import qualified Database.Persist as P


-- INVITES

type GetInvites = Get '[JSON] [Invite]
type PostInvite = Post '[JSON] InviteId
type DeleteInvite = Capture "inviteId" InviteId :> DeleteNoContent
type Invitees = Capture "inviteId" InviteId :> "invitees" :> InviteesAPI

type InvitesAPI
  = GetInvites
  :<|> PostInvite
  :<|> DeleteInvite
  :<|> Invitees


invitesHandler :: Email -> EventId -> ServerT InvitesAPI (AppT IO)
invitesHandler email int =
  getEventInvites email int
  :<|> postEventInvite email int
  :<|> deleteInvite email int
  :<|> inviteesHandler email int


getEventInvites :: Email -> EventId -> AppT IO [Invite]
getEventInvites = do
  checked $ \_ (EventId eventId) -> do
  pool <- asks dbPool
  let inviteQuery = select $ do
              event :& invite :& invitee <- 
                from $ table @DB.Event
                `innerJoin` table @DB.Invite
                `on` (\(event :& invite) ->
                    event ^. DB.EventId ==. invite ^. DB.InviteEvent)
                `leftJoin` table @DB.Invitee
                `on` (\(_ :& invite :& invitees) -> 
                    just (invite ^. DB.InviteId) ==. (invitees ?. DB.InviteeInvite))
              where_ $ event ^. DB.EventId ==. val (toSqlKey eventId)
              return (invite :& invitee)
  invites <- liftIO $ runSqlPool inviteQuery pool
  return $ groupInvites invites
  where
    groupInvites invites = uncurry dbInviteToApiInvite <$> toList (fromListWith (++) [(invite, maybe [] (\(Entity _ invitee)  -> [invitee]) mInvitee) | (Entity _ invite :& mInvitee) <- invites])



postEventInvite :: Email -> EventId -> AppT IO InviteId
postEventInvite email (EventId eventId) = do
  checkEventOrganizer email (EventId eventId)
  pool <- asks dbPool
  inviteCode <- generateNewInviteCode
  let newInvite = DB.Invite {
    DB.inviteEvent = toSqlKey eventId
    , DB.inviteCode = inviteCode
    }
  inviteId <- liftIO $ runSqlPool (insert newInvite) pool
  return $ InviteId $ fromSqlKey inviteId

deleteInvite :: Email -> EventId -> InviteId -> AppT IO NoContent
deleteInvite email (EventId eventId) (InviteId inviteId) = do 
  checkEventOrganizer email (EventId eventId)
  pool <- asks dbPool
  let query = P.delete (toSqlKey @DB.Invite inviteId)
  liftIO $ runSqlPool query pool
  return NoContent

-- UTILS

dbInviteToApiInvite :: DB.Invite -> [DB.Invitee] -> Invite
dbInviteToApiInvite DB.Invite{..} invitees =
  Invite {
    code = inviteCode
    , invitees = fmap (\DB.Invitee{..} -> Invitee {name = inviteeName, status = inviteeStatus} ) invitees
  }

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

