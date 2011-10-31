{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Upload (postUploadR) where
import Foundation
import Forms.Upload
import Data.Time
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import qualified Data.Text as T
import System.Cmd (system)

postUploadR :: Handler RepHtml
postUploadR = do
    ((res,widget),enctype) <- runFormPost uploadForm
    case res of 
        FormSuccess r -> do let fileInfo = img r
                            let name = T.unpack $ fileName fileInfo
                            let extension = getExtension name
                            let tag = T.unpack $ tags r
                            randName <- getRandomName extension
                            let fullName = randName ++ extension
                            if T.isPrefixOf "image" (fileContentType fileInfo) 
                                then do 
                                        time <- liftIO getCurrentTime
                                        liftIO $ L.writeFile (uploadDirectory ++ randName ++ extension) $ fileContent fileInfo
                                        liftIO $ system $ "convert " ++ uploadDirectory ++ randName ++ extension ++ " -thumbnail 100x100^ -gravity center -extent 100x100 " ++ uploadDirectory ++ randName ++ "-thumb" ++ extension
                                        id <- runDB (insert $ Images fullName tag time)
                                        redirect RedirectTemporary (ImageR id)
                                else do 
                                        setMessage "Not an image File"
                                        redirect RedirectTemporary RootR
        _ -> do setMessage "Form Error . Fill Again"
                redirect RedirectTemporary RootR 


