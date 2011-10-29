{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Upload (postUploadR) where
import Foundation
import Forms.Upload
import Data.Time
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import qualified Data.Text as T
postUploadR :: Handler RepHtml
postUploadR = do
    ((res,widget),enctype) <- runFormPost uploadForm
    case res of 
        FormSuccess r -> do let fileInfo = img r
                            let name = T.unpack $ fileName fileInfo
                            let extension = getExtension name
                            let tag = T.unpack $ tags r
                            randName <- getRandomName extension
                            if T.isPrefixOf "image" (fileContentType fileInfo) 
                                then do 
                                        time <- liftIO getCurrentTime
                                        liftIO $ L.writeFile (uploadDirectory ++ randName) $ fileContent fileInfo
                                        id <- runDB (insert $ Images randName tag time)
                                        redirect RedirectTemporary (ImageR id)
                                else do 
                                        setMessage "Not an image File"
                                        redirect RedirectTemporary RootR
        _ -> do setMessage "Form Error . Fill Again"
                redirect RedirectTemporary RootR 


