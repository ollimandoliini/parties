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
import Resources.Public
import System.IO (hSetBuffering)
import GHC.IO.Handle (BufferMode(LineBuffering))
import GHC.IO.Handle.FD (stdout)
import System.Directory
import WaiAppStatic.Types (LookupResult(..), unsafeToPiece)
import Network.Wai.Application.Static

type instance AddSetCookieApi (NoContentVerb 'DELETE) = Verb 'DELETE 204 '[JSON] (AddSetCookieApiVerb NoContent)
type instance AddSetCookieApi (NoContentVerb 'PUT) = Verb 'PUT 204 '[JSON] (AddSetCookieApiVerb NoContent)


type Admin = "api" :> "admin" :> "events" :> EventsAPI
type Public = "api" :> "public" :> PublicAPI

type API
  = Public
  :<|> (Auth '[JWT] JWTClaim :> Admin)

type APIAndFrontend = API :<|> Raw

protected :: AuthResult JWTClaim -> ServerT Admin (AppT IO)
protected (Authenticated claim) = eventsHandler (Email $ claimEmail claim)
protected _ = throwAll err401

unprotected :: ServerT Public (AppT IO)
unprotected
  = publicApiHandler

server :: ServerT APIAndFrontend (AppT IO)
server
  = (unprotected :<|> protected) :<|> serveFrontEnd "frontend"

serveFrontEnd :: String -> ServerT Raw (AppT IO)
serveFrontEnd rootDirectory =
  serveDirectoryWith settingsWithFallback
  where
    settingsWithFallback = (defaultWebAppSettings rootDirectory) { ssLookupFile = lookup' }
    lookup' p = do
      f <- ssLookupFile (defaultWebAppSettings rootDirectory) p
      case f of
        LRFile   f' -> return $ LRFile f'
        LRFolder f' -> return $ LRFolder f'
        LRNotFound -> ssLookupFile (defaultWebAppSettings rootDirectory) [unsafeToPiece "index.html"]


convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg application = Handler $ runReaderT (runApp application) cfg

api' :: Proxy APIAndFrontend
api' = Proxy

context :: Proxy '[CookieSettings, JWTSettings]
context = Proxy

appToServer :: Config -> Server APIAndFrontend
appToServer appCfg =
  hoistServerWithContext api' context (convertApp appCfg) server


app :: Config -> JWK -> JWKSet -> Application
app cfg key jwkSet =
  let jwtCfg = (defaultJWTSettings key) {validationKeys = jwkSet}
      cfg' = defaultCookieSettings :. jwtCfg :. EmptyContext
  in serveWithContext api' cfg' (appToServer cfg)

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
  hSetBuffering stdout LineBuffering
  lookupEnv "DB_CONNECTION_STRING" >>= print
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
    defaultConnectionString = "host=host.docker.internal port=5432 user=user password=password dbname=parties"

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env default' = do
  p <- lookupEnv env
  return $ maybe default' read p
