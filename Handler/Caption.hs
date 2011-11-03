{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Caption (postCaptionR) where
import Foundation
import Forms.Caption
import Helpers.Document
import Helpers.Storage 
import qualified Data.Text as T
postCaptionR :: ImagesId ->Handler RepHtml
postCaptionR id = do
    ((result, widget), enctype) <- runFormPost captionForm
    image <- getImagePersist id 
    case result of
        FormSuccess cap -> do 
                        let newImage = image { caption = cap}
                        updateImagePersist image newImage
                        redirect RedirectTemporary (ImageR id)
        _ ->  redirect RedirectTemporary $ ImageR id
    

