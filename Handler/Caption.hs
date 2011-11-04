{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Caption (postCaptionR) where
import Foundation
import Forms.Caption
import Helpers.Document
import Helpers.Storage 
import qualified Data.Text as T
postCaptionR :: SqlImageId ->Handler RepHtml
postCaptionR id = do
    ((result, widget), enctype) <- runFormPost captionForm
    maybeImage <- getImagePersist id 
    case result of
        FormSuccess cap -> do 
                        case maybeImage of 
                            Just image -> do 
                                let newImage = image { caption = cap}
                                updateImagePersist image newImage
                            _ -> return ()
                        redirect RedirectTemporary (ImageR id)
        _ ->  redirect RedirectTemporary $ ImageR id
    

