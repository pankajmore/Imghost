{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where
import Foundation
import Helpers.Document
import Database.Persist.Sqlite
import Handler.Root
import Handler.Upload
import Handler.Recent
import Handler.Search
import Handler.Display
import Handler.Image
import Handler.Vote
import Handler.Download
import Handler.Delete
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
import Settings
import qualified Database.Persist.Base
import Database.Persist.GenericSql (runMigration)


mkYesodDispatch "ImgHost" resourcesImgHost

withImgHost :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withImgHost conf logger f = do
#ifdef PRODUCTION
    s <- static Settings.staticDir
#else
    s <- staticDevel Settings.staticDir
#endif
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
            $ either error return . Database.Persist.Base.loadConfig
    Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        Database.Persist.Base.runPool dbconf (runMigration migrateAll) p
        Database.Persist.Base.runPool dbconf (runMigration migrateComments) p
        let h = ImgHost conf logger s p
        defaultRunner f h


-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withImgHost

