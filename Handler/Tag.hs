{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Tag (postTagR) where
import Foundation
import Forms.Tag
import Helpers.Document
import Helpers.Storage 
import qualified Data.Text as T
postTagR :: SqlImageId ->Handler RepHtml
postTagR id = do
    maybeImage <- getImagePersist id 
    case maybeImage of 
        Just image -> do 
            let tagList = T.splitOn "," $ tags image 
            ((result, widget), enctype) <- runFormPost (tagForm tagList)
            case result of
                FormSuccess modifiedTags -> do 
                                let newImage = image { tags = T.intercalate "," modifiedTags}
                                deleteTags id 
                                storeTags modifiedTags (created newImage) id
                                updateImagePersist image newImage
                _ ->  return ()
        _ -> return ()
    redirect RedirectTemporary (ImageR id)
    

