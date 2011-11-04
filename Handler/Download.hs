{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Download (postDownloadImageR) where
import Foundation
import Forms.Image
import qualified Data.ByteString.Lazy as L
import Helpers.Storage 
import Helpers.Document
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
postDownloadImageR :: SqlImageId -> Handler ()
postDownloadImageR id = do
    ((downresult, downwidget), downenctype) <- runFormPost (imageForm images_download_jpg)
    case downresult of
        FormSuccess _ -> do 
            maybeImage <- getImagePersist id
            case maybeImage of 
                Just image ->do 
                    let filePath = T.unpack $  T.append uploadDirectory  (name image)
                    let ext = getExtension $ name image
                    let cType    = T.encodeUtf8 $ T.append "image/" (T.tail ext)
                    setHeader "Content-Disposition" (T.encodeUtf8 $ T.concat [ "attachment; filename="
                                                           ,caption image,ext ])
                    sendFile cType filePath
                _ -> redirect RedirectTemporary (ImageR id)
        _ -> redirect RedirectTemporary (ImageR id)
