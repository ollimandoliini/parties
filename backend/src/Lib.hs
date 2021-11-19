{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH ( deriveJSON )
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server

import Data.Proxy
import Control.Monad.IO.Class

import Control.Monad.Reader (ReaderT, runReaderT, lift, MonadReader, MonadIO, asks)
import Control.Monad.Except         

import System.Environment (lookupEnv)

import qualified Data.ByteString as BS
import Database.Persist.Postgresql
       (ConnectionPool, ConnectionString, createPostgresqlPool)

import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)

import Data.Maybe (fromMaybe)
import           Control.Monad.Logger        (runStdoutLoggingT)

import Invites
import Auth
import Parties

import Database (doMigrations)
import App



convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg app = Handler $ runReaderT (runApp app) cfg


type PartyAPI = InvitesAPI :<|> EnrollmentAPI :<|> PartiesAPI

api :: Proxy PartyAPI
api = Proxy


appToServer :: Config -> Server PartyAPI
appToServer cfg = hoistServerWithContext api (Proxy :: Proxy '[BasicAuthCheck User]) (convertApp cfg) server

app :: Config -> Application
app cfg = serveWithContext api ctx (appToServer cfg)
  where
    ctx :: Context '[BasicAuthCheck User]
    ctx = authCheck (adminUser cfg) (adminPassword cfg) :. EmptyContext


server :: ServerT PartyAPI (AppT IO)
server = inviteHandler :<|> enrollmentHandler :<|> partiesHandler




makePool :: BS.ByteString -> IO ConnectionPool
makePool dbString = runStdoutLoggingT (createPostgresqlPool dbString 1)

startApp :: IO ()
startApp = do
  !dbString <- lookupSetting "DB_CONNECTION_STRING" "host=localhost port=5432 user=user password=password dbname=parties"
  !adminUser <- lookupSetting "ADMIN_USER" "olli"
  !adminPw <- lookupSetting "ADMIN_PASSWORD" "yeahyeah"
  pool <- makePool dbString
  let cfg = Config pool adminUser adminPw
  runSqlPool doMigrations pool
  run 8080 (app cfg)


lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ maybe def read p
