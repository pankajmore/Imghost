{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Application where
import Foundation
import Settings
import Helpers.Document
import Network.Wai.Application.Static (defaultWebAppSettings)
import Handler.Root
import Handler.Upload
import Handler.Recent
import Handler.Search
import Handler.Display
import Handler.Image
import Handler.Vote
import Handler.Tag
import Handler.Download
import Handler.Delete
import Handler.Api
import Handler.Profile
import Handler.Json
import Yesod.Auth
import Handler.Caption
import Yesod.Comments.Management
import Yesod.Comments.Storage
import Data.Dynamic (Dynamic, toDyn)
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger)
import Data.ByteString (ByteString)
import qualified Database.Persist.Base as Base
import Database.Persist.GenericSql (runMigration)

mkYesodDispatch "ImgHost" resourcesImgHost

withImgHost :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withImgHost conf logger f = do
    {-withSqlitePool "test.db3" openConnectionCount $ \pool -> do-}
        {-runSqlPool (runMigration migrateAll) pool-}
        {-runSqlPool (runMigration migrateComments) pool-}
        {-let h = ImgHost conf logger (Static defaultWebAppSettings) pool-}
        {-defaultRunner f h-}
    {-dbconf <- withYamlEnvironment "config/postgres.yml" (appEnv conf)-}
            {-$ either error return . Database.Persist.Base.loadConfig-}
    {-Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do-}
        {-Database.Persist.Base.runPool dbconf (runMigration migrateAll) p-}
        {-let h = ImgHost conf logger (Static defaultWebAppSettings) p-}
        {-defaultRunner f h-}
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
            $ either error return . Base.loadConfig

    Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        Base.runPool dbconf (runMigration migrateAll)    p
        Base.runPool dbconf (runMigration migrateComments) p
        let h = ImgHost conf logger (Static defaultWebAppSettings) p
        defaultRunner f h

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withImgHost

