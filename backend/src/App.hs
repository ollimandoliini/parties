{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks)
import Control.Monad.Except ( MonadIO, MonadError, ExceptT )         
import qualified Data.ByteString as BS

import Servant (ServerError)
import Database.Persist.Postgresql
       (ConnectionPool, ConnectionString, createPostgresqlPool)


data Config = Config {
  dbPool :: ConnectionPool
} deriving (Show)

newtype AppT m a
    = AppT
    { runApp :: ReaderT Config (ExceptT ServerError m) a
    } deriving
    ( Functor, Applicative, Monad,
    MonadReader Config, MonadError ServerError, MonadIO
    )