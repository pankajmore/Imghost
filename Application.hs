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
import Handler.Profile
import Yesod.Auth
import Yesod.Comments.Management
import Yesod.Comments.Storage
import Data.Dynamic (Dynamic, toDyn)

mkYesodDispatch "ImgHost" resourcesImgHost
mainp :: IO ()
mainp = withSqlitePool "test.db3" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    runSqlPool (runMigration migrateComments) pool
    warpDebug 5432 $ ImgHost (Static defaultWebAppSettings) pool


-- for yesod devel
{-withDevelAppPort :: Dynamic-}
{-withDevelAppPort = toDyn $ defaultDevelApp withDevSite-}

