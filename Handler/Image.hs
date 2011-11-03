{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Image (getImageR,postImageR) where
import Foundation
import Forms.Upload
import Data.Time
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Yesod.Comments
import Forms.Image
import Forms.Caption 
import Yesod.Goodies.Time
getImageR :: ImagesId -> Handler RepHtml
getImageR id = do 
                ((iresult, iwidget), ienctype) <- generateFormPost (imageForm images_thumbsup_jpg)
                ((delresult, delwidget), delenctype) <- generateFormPost (imageForm images_delete_png)
                ((dresult, dwidget), denctype) <- generateFormPost (imageForm images_thumbsdown_jpg)
                ((downresult, downwidget), downenctype) <- generateFormPost (imageForm images_download_jpg)
                ((capresult, capwidget), capenctype) <- generateFormPost captionForm
                maybeImage <- getImagePersist id
                maybeuid <- maybeAuthId
                boolDeleteImage <- case maybeuid of
                                        Nothing -> return False
                                        Just uid -> canDeleteImage uid id
                (boolIVote,boolDVote) <- case maybeuid of
                                            Nothing ->  return (True,True)
                                            Just uid -> do
                                                iVote <- canIVote uid id 
                                                dVote <- canDVote uid id 
                                                return (iVote,dVote)
                case maybeImage of
                    Just image ->do 
                                    createdTime <- humanReadableTime $ created image
                                    let iName = name image 
                                    defaultLayout $ do
                                        urlbox <- lift newIdent
                                        tableProperty <- lift newIdent
                                        $(widgetFile "image")
                                        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
                                        addScript $ StaticR js_hideSubmit_js
                                        addCommentsAuth $ name image
                    Nothing -> do setMessage "ID not found in the database"
                                  redirect RedirectTemporary RootR

postImageR = getImageR

