{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Foundation 
    ( ImgHost(..)
    , module Yesod
    ) where
import Settings.StaticFiles
import Yesod
import Models(Img(..))
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class
import Yesod.Static
import Network.Wai.Application.Static (defaultWebAppSettings)
import Database.Persist.Sqlite
import System.Random
import Data.Time
------------------------------------------------------------------------------------------------------------------------------
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Images
    imageName String
    imageTag String
    created UTCTime
    ImageName imageName
|]

data ImgHost = ImgHost 
        { getStatic :: Static
        , connPool  :: ConnectionPool
        }
instance Yesod ImgHost where
    approot _ = ""
instance YesodPersist ImgHost where
    type YesodPersistBackend ImgHost = SqlPersist
    runDB action = liftIOHandler $ do
            ImgHost _ pool <- getYesod
            runSqlPool action pool

instance RenderMessage ImgHost FormMessage where
    renderMessage _ _ = defaultFormMessage
mkYesod "ImgHost" $(parseRoutesFile "routes")
------------------------------------------------------------------------------------------------------------------------------
--Helpers and Configs 
openConnectionCount :: Int
openConnectionCount = 10
uploadDirectory :: String
uploadDirectory = "./static/upload/"
sUploadDirectory :: String
sUploadDirectory = tail uploadDirectory
defaultTags :: [(Text,Text)]
defaultTags = [("Nature","Nature"),("Celebrity","Celebrity"),("Machines","Machines"),("Other","Other")]
{-
 - Generates a random name of length 20 containing chars from a-z, checks if
 - the database already contains that name, if calls itself again to do another
 - generation and check until it finds a unique name.
-}
getRandomName :: String -> GHandler ImgHost ImgHost String
getRandomName ending = do  
                    gen <- liftIO getStdGen  
                    let a = take 20 (randomRs ('a','z') gen) 
                    liftIO $ newStdGen
                    let isInDatabase = False
                    isInDatabase <- checkDatabase (a ++ ending)
                    if (not isInDatabase)
                        then return (a ++ ending)
                        else getRandomName ending
checkDatabase :: String -> GHandler ImgHost ImgHost Bool
checkDatabase image = do 
           maybeImage <- runDB (getBy $ ImageName image)
           case maybeImage of
               Just _ -> return True
               _ -> return False
--------------------------------------------------------------------------------------------------------------------------------

uploadForm :: Html -> Form ImgHost ImgHost (FormResult Img, Widget)
uploadForm = renderTable $ Img
    <$> fileAFormReq "Upload Image"
    <*> areq (selectField defaultTags) "Tags" Nothing

--------------------------------------------------------------------------------------------------------------------------------
postUploadR :: Handler RepHtml
postUploadR = do
    ((res,widget),enctype) <- runFormPost uploadForm
    case res of 
        FormSuccess r -> do let fileInfo = img r
                            let name = T.unpack $ fileName fileInfo
                            let extension = dropWhile (/='.') name
                            let tag = T.unpack $ tags r
                            randName <- getRandomName extension
                            if T.isPrefixOf "image" (fileContentType fileInfo) 
                                then do 
                                        time <- liftIO getCurrentTime
                                        liftIO $ L.writeFile (uploadDirectory ++ randName) $ fileContent fileInfo
                                        runDB (insert $ Images randName tag time)
                                else do 
                                        setMessage "Not an image File"
                                        redirect RedirectTemporary RootR
                            let image = sUploadDirectory++randName
                            defaultLayout [whamlet|
<a href=#{image}>Image
|]
        _ -> do setMessage "Form Error . Fill Again"
                redirect RedirectTemporary RootR 
getRootR :: Handler RepHtml
getRootR = do
    ((_,widget),enctype) <- runFormPost uploadForm
    defaultLayout $ do 
        setTitle "Home"
        [whamlet|
<form method=post action=@{UploadR} enctype=#{enctype}>
    ^{widget}        
    <input type=submit>
|]
main :: IO ()
main = withSqlitePool "test.db3" openConnectionCount $ \pool -> do
    time <- liftIO getCurrentTime
    runSqlPool (runMigration migrateAll) pool
    warpDebug 5432 $ ImgHost (Static defaultWebAppSettings) pool
