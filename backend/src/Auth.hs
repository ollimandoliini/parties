{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Control.Lens ( (^.) )
import qualified Crypto.JWT as Jose
import qualified Data.Map as M
import Data.Text ( Text, pack )
import GHC.Generics ( Generic )
import Servant.Auth.Server ( FromJWT(..), ToJWT )
import Data.Aeson (Result(Success, Error), ToJSON, fromJSON)

newtype JWTClaim = JWTClaim {
    claimEmail :: Text
  } deriving (Show, Generic)

instance ToJSON JWTClaim
instance ToJWT JWTClaim

instance FromJWT JWTClaim where
  decodeJWT m = case M.lookup "https://eventit.fi/email" (m ^. Jose.unregisteredClaims) of
    Nothing -> Left "Missing 'https://eventit.fi/email' claim"
    Just v -> case fromJSON v of
      Error e -> Left $ pack e
      Success a -> Right $ JWTClaim a
