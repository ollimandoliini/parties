{-# LANGUAGE OverloadedStrings #-}
module Database.Migrations where

import           Control.Monad.Reader                (ReaderT, liftIO)
import           Data.Text
import           Data.Text.IO                        (readFile)
import           Database.Persist.Migration
import           Database.Persist.Migration.Postgres (runMigration)
import           Database.Persist.Postgresql         (rawExecute)
import           Database.Persist.Sql                (SqlBackend)
import           Prelude                             hiding (readFile)


getSql :: Int -> IO Text
getSql migrationId = do
  readFile ("./migrations/migration_" <> show migrationId <> ".sql")

createMigration :: Text -> Text -> Operation
createMigration description sql =
    RawOperation description (rawExecute sql [] >> return [])

createMigrations :: [Operation] -> Migration
createMigrations operations =
  let createMigration' operationSet fromVersion toVersion = fromVersion ~> toVersion := [operationSet]
  in fmap (uncurry3 createMigration') (zip3 operations [0..] [1..])


migrations :: [(Int, Text)]
migrations = [
  (1, "Create events, invites and invitees tables")
  ]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = do
    sqls <- liftIO $ mapM (\(version, description) -> getSql version >>= (\sqlText' -> return (description, sqlText'))) migrations
    let migrations' = createMigrations $ fmap (uncurry createMigration) sqls
    runMigration defaultSettings migrations'

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

