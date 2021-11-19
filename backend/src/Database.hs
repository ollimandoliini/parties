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

Party json
    name Text
    description Text
    deriving Show Eq

Invite json
    party PartyId
    invitee [ParticipantId]
    deriving Show Eq

Participant json
    name Text
    invite InviteId
    deriving Show Eq

|]

doMigrations :: SqlPersistT IO ()
doMigrations = do
    runMigration migrateAll