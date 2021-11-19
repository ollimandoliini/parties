{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parties where

import Servant
import Data.Aeson.TH
import Control.Monad.Reader (ask, MonadIO (liftIO), asks)
import Database.Persist
import Database.Persist.Sql (runSqlPool, fromSqlKey)



import Auth
import App
import qualified Database (Party(..))
import Data.Text


data Party = Party {
    partyId :: String
    , partyName :: Text
    , description :: Text
}
$(deriveJSON defaultOptions ''Party)

type PartiesAPI = BasicAuth "parties" User :>
    "parties" :>
        (Get '[JSON] [Entity Database.Party] :<|>
        ReqBody '[JSON] Party :> Post '[JSON] Int :<|>
        Capture "id" String :> DeleteNoContent)


partiesHandler :: ServerT PartiesAPI (AppT IO)
partiesHandler user = getPartiesHandler user :<|> postPartyHandler user :<|> deletePartyHandler user
    where
    deletePartyHandler user id' = return NoContent

getPartiesHandler :: User -> AppT IO [Entity Database.Party]
getPartiesHandler user = do
    pool <- asks dbPool
    liftIO $ runSqlPool (selectList [] []) pool


postPartyHandler :: User -> Party -> AppT IO Int
postPartyHandler user party = do
    pool <- asks dbPool
    partyId <- liftIO $ runSqlPool (insert newParty) pool
    return $ fromIntegral $ fromSqlKey partyId
    where
        newParty = Database.Party (partyName party) (description party)



