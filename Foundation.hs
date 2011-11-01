{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Foundation 
    ( ImgHost(..)
    , ImgHostRoute (..)
    , resourcesImgHost
    , openConnectionCount
    , uploadDirectory
    , sUploadDirectory
    , defaultTags
    , Widget
    , Handler
    , module Settings
    , module Settings.StaticFiles
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
import Yesod.Comments
import Yesod.Comments.Management
import Yesod.Comments.Storage



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data ImgHost = ImgHost 
        { getStatic :: Static
        , connPool  :: ConnectionPool
        }
mkYesodData "ImgHost" $(parseRoutesFile "routes")
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
instance Yesod ImgHost where
    approot _ = "http://localhost:5432"
    maximumContentLength _ (Just (UploadR {})) = 32 * 1024 * 1024
    maximumContentLength _ _ = 2 * 1024 * 1024
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

instance YesodComments ImgHost where
    getComment     = getCommentPersist
    storeComment   = storeCommentPersist
    deleteComment  = deleteCommentPersist
    loadComments   = loadCommentsPersist
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
openConnectionCount :: Int
openConnectionCount = 10
uploadDirectory :: String
uploadDirectory = "./static/upload/"
sUploadDirectory :: String
sUploadDirectory = tail uploadDirectory
defaultTags :: [(Text,Text)]
defaultTags = [("Nature","Nature"),("Celebrity","Celebrity"),("Machines","Machines"),("NSFW","NSFW"),("Other","Other")]

