{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Upload (postUploadR) where
import Foundation
import Forms.Upload
import Helpers.Document
import Helpers.Storage
import qualified Data.Text as T
import System.Cmd (system)

postUploadR :: Handler RepHtml
postUploadR = do
    ((res,widget),enctype) <- runFormPost uploadForm
    case res of 
        FormSuccess formImage -> do 
                            let fileInfo = img formImage
                            let extension = getExtension $ fileName fileInfo
                            let fullName = randName ++ extension
                            if T.isPrefixOf "image" (fileContentType fileInfo) 
                                then do 
                                        randName <- getRandomName extension
                                        time <- liftIO getCurrentTime
                                        mid <- maybeAuthId
                                        let image = { name = fileName fileInfo
                                                    , tag = tag formImage 
                                                    , owner = mid 
                                                    , caption = getName $ fileName fileInfo
                                                    , votes = 0 
                                                    , created = time
                                                    }
                                        liftIO $ L.writeFile (T.append uploadDirectory randName) $ fileContent fileInfo
                                        liftIO $ system $ T.concat ["convert ",uploadDirectory,randName," -thumbnail 100x100^ -gravity center -extent 100x100 ",uploadDirectory,getThumb randName]
                                        id <- storeImagePersist image
                                        redirect RedirectTemporary $ ImageR id
                                else do 
                                        setMessage "Not an image File"
                                        redirect RedirectTemporary RootR
        _ -> do setMessage "Form Error . Fill Again"
                redirect RedirectTemporary RootR 


