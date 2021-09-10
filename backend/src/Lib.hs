{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Proxy
import Control.Monad.IO.Class

import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either   (EitherT, left)



data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

data Config = Config {
  field1 :: String
  , field2 :: String
}

type AppM = ReaderT Config (EitherT ServantErr IO)

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = do
  liftIO $ print "moro"
  return users


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
