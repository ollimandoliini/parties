{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Lib
  ( startApp,
    app,
  )
where

import           App
import           Control.Monad.Logger                      (runStdoutLoggingT)
import           Control.Monad.Reader                      (runReaderT)
import qualified Data.ByteString                           as BS
import           Database                                  (doMigrations)
import           Database.Persist.Postgresql               (ConnectionPool,
                                                            createPostgresqlPool)
import           Database.Persist.Sql                      (runSqlPool)
import           Network.Wai                               (Middleware,
                                                            Request (requestHeaders))
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.Cors               (CorsResourcePolicy (corsMethods, corsRequestHeaders),
                                                            cors,
                                                            simpleCorsResourcePolicy)
import           Resources.Event
import           Servant
import           System.Environment                        (lookupEnv)

import           Auth                                      (JWTClaim (claimEmail))
import           Servant.Auth.Server

import           Crypto.JOSE                               (JWKSet)
import           Crypto.JOSE.JWK                           (JWK)
import           Network.HTTP.Client.Conduit               (responseBody)
import           Network.HTTP.Simple                       (httpJSON)
import           Servant.Auth.Server.Internal.AddSetCookie (AddSetCookieApi,
                                                            AddSetCookieApiVerb)
import           Types                                     (Email (Email))


type instance AddSetCookieApi (NoContentVerb 'DELETE) = Verb 'DELETE 204 '[JSON] (AddSetCookieApiVerb NoContent)
type instance AddSetCookieApi (NoContentVerb 'PUT) = Verb 'PUT 204 '[JSON] (AddSetCookieApiVerb NoContent)

type Protected = "events" :> EventsAPI

protected :: Servant.Auth.Server.AuthResult JWTClaim -> ServerT Protected (AppT IO)
protected (Servant.Auth.Server.Authenticated claim) = eventsHandler (Email $ claimEmail claim)
protected _ = throwAll err401

type Unprotected = Raw


unprotected :: CookieSettings -> JWTSettings -> ServerT Unprotected (AppT IO)
unprotected _ _ = serveDirectoryFileServer "frontend"

type API = (Auth '[JWT] JWTClaim :> Protected) :<|> Unprotected

server :: CookieSettings -> JWTSettings -> ServerT API (AppT IO)
server cs jwts = protected :<|> unprotected cs jwts

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg application = Handler $ runReaderT (runApp application) cfg


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
debug application req resp = do
  putStrLn "Request headers:"
  print (requestHeaders req)
  application req resp

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
lookupSetting env default' = do
  p <- lookupEnv env
  return $ maybe default' read p
