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
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)

share
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"
    ] [persistLowerCase|

User json
    name Text
    passwordHash Text
    email Text
    UniqueEmail email
    deriving Show Eq

Event json
    name Text
    organizer UserId
    description Text
    invite 
    deriving Show Eq

Invite json
    event EventId
    code Text
    deriving Show Eq

Invitee json
    invite InviteId
    name Text
    deriving Show Eq
|]

doMigrations :: SqlPersistT IO ()
doMigrations = do
    runMigration migrateAll

