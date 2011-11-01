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
import qualified Data.ByteString.Char8 as C
postDownloadImageR :: ImagesId -> Handler ()
postDownloadImageR id = do
    ((downresult, downwidget), downenctype) <- runFormPost (imageForm images_download_jpg)
    case downresult of
        FormSuccess _ -> do 
            im <- getImage id
            case im of 
                Just (iName,tag,caption,votes,cTime) ->do 
                    let filePath = uploadDirectory++iName
                    let cType    = C.pack $ "image/" ++ (tail $ getExtension iName)
                    setHeader "Content-Disposition" (T.encodeUtf8.T.concat $ map T.pack [ "attachment; filename="
                                                           ,caption,getExtension iName  ])
                    sendFile cType filePath
                _ -> redirect RedirectTemporary (ImageR id)
        _ -> redirect RedirectTemporary (ImageR id)
