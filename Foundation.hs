{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Foundation 
    ( ImgHost(..)
    , ImgHostRoute (..)
    , Images
    , resourcesImgHost
    , openConnectionCount
    , uploadDirectory
    , sUploadDirectory
    , defaultTags
    , Widget
    , Handler
    , module Settings
    , module Yesod
    , module Models
    , module Yesod.Static
    )where
import Settings.StaticFiles
import Settings
import Yesod
import Models
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Yesod.Static
import Database.Persist.Sqlite
import Text.Hamlet (hamletFile)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data ImgHost = ImgHost 
        { getStatic :: Static
        , connPool  :: ConnectionPool
        }
mkYesodData "ImgHost" $(parseRoutesFile "routes")
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
instance Yesod ImgHost where
    approot _ = ""
    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            $(widgetFile "header")
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
            $(widgetFile "footer")
        hamletToRepHtml $(hamletFile "hamlet/default-layout-wrapper.hamlet")

instance RenderMessage ImgHost FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist ImgHost where
    type YesodPersistBackend ImgHost = SqlPersist
    runDB action = liftIOHandler $ do
            ImgHost _ pool <- getYesod
            runSqlPool action pool
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
openConnectionCount :: Int
openConnectionCount = 10
uploadDirectory :: String
uploadDirectory = "./static/upload/"
sUploadDirectory :: String
sUploadDirectory = tail uploadDirectory
defaultTags :: [(Text,Text)]
defaultTags = [("Nature","Nature"),("Celebrity","Celebrity"),("Machines","Machines"),("Other","Other")]
