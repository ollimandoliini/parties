{-# LANGUAGE NamedFieldPuns #-}
module Utils where
import Data.Text (Text)
import App (AppT, Config (dbPool))
import qualified Database as DB
import Database.Persist (Key, PersistStoreRead (get))
import Database.Persist.Sql (toSqlKey, runSqlPool)
import Control.Monad.Except (throwError)
import Servant (err404)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import Types (Email (Email), EventId (EventId))

checkEventOrganizer :: Email -> EventId -> AppT IO ()
checkEventOrganizer (Email email) (EventId eventId) = do
  pool <- asks dbPool
  DB.Event{ DB.eventOrganizer } <- or404 . liftIO $ runSqlPool (get eventIdKey) pool
  if eventOrganizer == email
    then return ()
    else throwError err404
  where
    eventIdKey :: Key DB.Event
    eventIdKey = toSqlKey $ fromIntegral eventId

checked :: (Email -> EventId -> AppT IO a) -> (Email -> EventId -> AppT IO a)
checked handler email int = checkEventOrganizer email int *> handler email int

or404 :: AppT IO (Maybe b) -> AppT IO b
or404 res = res >>= maybe (throwError err404) pure
