module Auth where

import qualified Data.ByteString as BS
import Servant


newtype User = User
  { userName :: BS.ByteString
  } deriving (Eq, Show)

authCheck :: BS.ByteString -> BS.ByteString ->  BasicAuthCheck User
authCheck adminUser adminPassword =
  let check (BasicAuthData username password) =
        if username == adminUser && password == adminPassword
        then return (Authorized (User username))
        else return Unauthorized
  in BasicAuthCheck check