{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
  ( startApp,
    app,
  )
where

-- import Invites
-- import Auth

import App
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, lift, runReaderT)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Database (doMigrations)
import Database.Persist.Postgresql
  ( ConnectionPool,
    ConnectionString,
    createPostgresqlPool,
  )
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import GHC.Generics
import Network.Wai
    ( Application, Request(requestHeaders), Middleware )
import Network.Wai.Handler.Warp
import Resources.Event
import Servant
import Servant.Server
import System.Environment (lookupEnv)
import qualified Database as DB
import Database.Persist
import Data.Text (Text)
import Network.Wai.Middleware.Cors

import Servant.Auth.Server
import Auth (JWTClaim(claimEmail))

import Crypto.JOSE.JWK ( JWK )
import Debug.Trace (trace)
import Crypto.JOSE (JWKSet)
import Network.HTTP.Simple (httpJSON, Response)
import Network.HTTP.Client.Conduit (responseBody)

type Protected = PartiesAPI

protected :: Servant.Auth.Server.AuthResult JWTClaim -> ServerT Protected (AppT IO)
protected (Servant.Auth.Server.Authenticated claim) = eventsHandler (claimEmail claim)
protected x = trace ("here:" <> show x) throwAll err401

type Unprotected = Raw


unprotected :: CookieSettings -> JWTSettings -> ServerT Unprotected (AppT IO)
unprotected cs jwts = serveDirectoryFileServer "frontend"

type API = (Auth '[JWT] JWTClaim :> Protected) :<|> Unprotected

server :: CookieSettings -> JWTSettings -> ServerT API (AppT IO)
server cs jwts = protected :<|> unprotected cs jwts

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg app = Handler $ runReaderT (runApp app) cfg


api' :: Proxy API
api' = Proxy

appToServer :: Config -> CookieSettings -> JWTSettings -> Server API
appToServer appCfg cookieSettings jwtSettings =
  hoistServerWithContext api' (Proxy :: Proxy '[CookieSettings, JWTSettings]) (convertApp appCfg) (server cookieSettings jwtSettings)


corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Authorization", "Content-Type"]
        , corsMethods = ["OPTIONS", "GET", "PUT", "POST", "PATCH", "DELETE"] }

app :: Config -> JWK -> JWKSet -> Application
app cfg key jwkSet =
  let jwtCfg = (defaultJWTSettings key) {validationKeys = jwkSet}
      cookieSettings = defaultCookieSettings {cookieIsSecure = NotSecure, cookieXsrfSetting = Nothing}
      cfg' = cookieSettings :. jwtCfg :. EmptyContext
  in corsPolicy $ serveWithContext api' cfg' (appToServer cfg cookieSettings jwtCfg)

debug :: Middleware
debug app req resp = do { putStrLn "Request headers:" ; print (requestHeaders req) ; app req resp }

makePool :: BS.ByteString -> IO ConnectionPool
makePool dbString = runStdoutLoggingT (createPostgresqlPool dbString 1)


getJwk :: IO JWKSet
getJwk = responseBody <$> httpJSON jwkUrl
  where
    jwkUrl = "https://dev-7xdjfw10.eu.auth0.com/.well-known/jwks.json"


startApp :: IO ()
startApp = do
  !dbString <- lookupSetting "DB_CONNECTION_STRING" defaultConnectionString
  !key <- generateKey
  !jwkSet <- getJwk
  pool <- makePool dbString
  let cfg = Config pool
      port = 8080
  runSqlPool doMigrations pool
  putStrLn $ "App running in port: " ++ show port
  run port (debug $ app cfg key jwkSet)
  where
    defaultConnectionString = "host=localhost port=5432 user=user password=password dbname=parties"

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  p <- lookupEnv env
  return $ maybe def read p
