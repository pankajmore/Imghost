{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Download (postDownloadImageR) where
import Foundation
import Forms.Image
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as C
postDownloadImageR :: ImagesId -> Handler ()
postDownloadImageR id = do
    ((downresult, downwidget), downenctype) <- runFormPost (imageForm images_download_jpg)
    case downresult of
        FormSuccess _ -> do 
            maybeImage <- getImagePersist id
            case maybeImage of 
                Just image ->do 
                    let filePath = uploadDirectory ++ (name image)
                    let ext = getExtension $ name image
                    let cType    = encodeUtf8 $ T.append "image/" (T.tail ext)
                    setHeader "Content-Disposition" (T.encodeUtf8.T.concat $ map T.pack [ "attachment; filename="
                                                           ,caption,ext ])
                    sendFile cType filePath
                _ -> redirect RedirectTemporary (ImageR id)
        _ -> redirect RedirectTemporary (ImageR id)
