{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Upload (postUploadR) where
import Foundation
import Forms.Upload
import Helpers.Document
import Helpers.Storage
import qualified Data.Text as T
import System.Cmd (system)
import Data.Time (getCurrentTime)
import Data.List (intersperse)
import qualified Data.ByteString.Lazy as L
import Data.Aeson
postUploadR :: Handler RepJson
postUploadR = do
    ((res,widget),enctype) <- runFormPost uploadForm
    case res of 
        FormSuccess formImage -> do 
                            let fileInfo = iimg formImage
                            let extension = getExtension $ fileName fileInfo
                            if T.isPrefixOf "image" (fileContentType fileInfo) 
                                then do 
                                        randName <- getRandomName extension
                                        time <- liftIO getCurrentTime
                                        mid <- maybeAuthId
                                        master <- getYesod
                                        let image = Image { name = randName
                                                    , tags = T.intercalate "," $ itags formImage 
                                                    , owner = mid 
                                                    , caption = getName $ fileName fileInfo
                                                    , votes = 0 
                                                    , hits = 0
                                                    , created = time
                                                    }
                                        liftIO $ L.writeFile (T.unpack $ T.append uploadDirectory randName) $ fileContent fileInfo
                                        liftIO $ system . T.unpack  $ T.concat ["convert ",uploadDirectory,randName," -thumbnail 100x100^ -gravity center -extent 100x100 ",uploadDirectory,getThumb randName]
                                        id <- storeImagePersist image
                                        jsonToRepJson.toJSON $ (\(a,b) -> toJsonImage (yesodRender master (ImageR a) [] ) b) (id,image)

                                else do 
                                        setMessage "Not an image File"
                                        redirect RedirectTemporary RootR
        _ -> do setMessage "Form Error . Fill Again"
                redirect RedirectTemporary RootR 


