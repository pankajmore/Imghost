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
import Data.Dynamic (Dynamic, toDyn)
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger)
import Data.ByteString (ByteString)

mkYesodDispatch "ImgHost" resourcesImgHost
{-mainp :: IO ()-}
{-mainp = withSqlitePool "test.db3" openConnectionCount $ \pool -> do-}
    {-runSqlPool (runMigration migrateAll) pool-}
    {-warpDebug 5432 $ ImgHost (Static defaultWebAppSettings) pool-}

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.

withImgHost :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withImgHost conf logger f = do
    withSqlitePool "test.db3" openConnectionCount $ \pool -> do
        runSqlPool (runMigration migrateAll) pool
        let h = ImgHost conf logger (Static defaultWebAppSettings) pool
        defaultRunner f h



-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withImgHost

