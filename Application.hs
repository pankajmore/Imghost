{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Application where
import Foundation
import Helpers.Document
import Network.Wai.Application.Static (defaultWebAppSettings)
import Database.Persist.Sqlite
import Handler.Root
import Handler.Upload

mkYesodDispatch "ImgHost" resourcesImgHost
main :: IO ()
main = withSqlitePool "test.db3" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    warpDebug 5432 $ ImgHost (Static defaultWebAppSettings) pool
