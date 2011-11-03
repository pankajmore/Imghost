{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Application where
import Foundation
import Helpers.Document
import Network.Wai.Application.Static (defaultWebAppSettings)
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

mkYesodDispatch "ImgHost" resourcesImgHost

withImgHost :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withImgHost conf logger f = do
    withSqlitePool "test.db3" openConnectionCount $ \pool -> do
        runSqlPool (runMigration migrateAll) pool
        runSqlPool (runMigration migrateComments) pool
        let h = ImgHost conf logger (Static defaultWebAppSettings) pool
        defaultRunner f h


-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withImgHost

