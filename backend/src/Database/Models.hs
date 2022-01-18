{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.Models where

import           Data.Text
import           Database.Persist.TH                 (mkPersist,
                                                      persistLowerCase, share,
                                                      sqlSettings)
import           Prelude                             hiding (readFile)

import           Data.Time
import           Types                               (Status (..))

share
    [ mkPersist sqlSettings
    ] [persistLowerCase|

Event json
    name Text
    organizer Text
    description Text
    startTime UTCTime
    location Text
    invitesArePublic Bool
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
