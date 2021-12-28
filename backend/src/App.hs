{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import           Control.Monad.Except        (ExceptT, MonadError, MonadIO)
import           Control.Monad.Reader        (MonadReader, ReaderT)

import           Database.Persist.Postgresql (ConnectionPool)
import           Servant                     (ServerError)

newtype Config = Config {
  dbPool :: ConnectionPool
} deriving (Show)

newtype AppT m a
    = AppT
    { runApp :: ReaderT Config (ExceptT ServerError m) a
    } deriving
    ( Functor, Applicative, Monad,
    MonadReader Config, MonadError ServerError, MonadIO
    )
