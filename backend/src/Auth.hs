{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Control.Lens ( (^.) )
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import qualified Crypto.JWT as Jose
import Data.Aeson ( fromJSON, Result(Success, Error), ToJSON )
import Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Strict as KM
import Data.Text ( Text, pack )
import qualified Database as DB
import Database.Persist.Postgresql (runSqlPool)
import Debug.Trace (trace)
import GHC.Generics ( Generic )
import Servant( Header, NoContent (NoContent), err401, throwError)
import Servant.API (Headers)
import Servant.Auth.Server ( FromJWT(..), ToJWT )

newtype JWTClaim = JWTClaim {
    claimEmail :: Text
  } deriving (Show, Generic)

instance ToJSON JWTClaim
instance ToJWT JWTClaim

instance FromJWT JWTClaim where
  decodeJWT m = case KM.lookup "https://eventit.fi/email" (m ^. Jose.unregisteredClaims) of
    Nothing -> Left "Missing 'https://eventit.fi/email' claim"
    Just v -> case fromJSON v of
      Error e -> Left $ pack e
      Success a -> Right $ JWTClaim a
