{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database where

import Data.Text
import Database.Persist.TH
       (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (SqlPersistT, runMigration)
import Data.Time
import Types (Status(..))

share
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"
    ] [persistLowerCase|

Event json
    name Text
    organizer Text
    description Text
    startTime UTCTime
    location Text
    deriving Show Eq

Invite json
    event EventId OnDeleteCascade
    code Text
    deriving Show Eq Ord

Invitee json
    invite InviteId OnDeleteCascade
    name Text
    status Status
    deriving Show Eq
|]

doMigrations :: SqlPersistT IO ()
doMigrations = do
    runMigration migrateAll

