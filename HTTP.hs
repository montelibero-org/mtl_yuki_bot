{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module HTTP where

import           Control.Lens (view)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime, addUTCTime, getCurrentTime,
                            secondsToNominalDiffTime)
import qualified Database.Persist as Persist
import           Database.Persist.Sqlite (runMigration, runSqlite)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import qualified Network.Wreq as Wreq

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Cache
      Id            Text
      responseBody  ByteString
      updated       UTCTime
  |]

getCached :: Text -> IO ByteString
getCached url =
  runSqlite cacheFile do
    runMigration migrateAll
    cached <- Persist.get (CacheKey url)
    now <- liftIO getCurrentTime
    case cached of
      Just Cache{cacheResponseBody, cacheUpdated}
        | addUTCTime timeout cacheUpdated > now ->
            pure cacheResponseBody
      _ -> do
        responseBody <-
          liftIO $
          BSL.toStrict . view Wreq.responseBody <$> Wreq.get (Text.unpack url)
        Persist.repsert
          (CacheKey url)
          Cache{cacheResponseBody = responseBody, cacheUpdated = now}
        pure responseBody
  where
    cacheFile = "http_cache.sqlite"
    timeout = secondsToNominalDiffTime 60
