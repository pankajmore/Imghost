{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Caption (postCaptionR) where
import Foundation
import Forms.Caption
import Helpers.Document
import qualified Data.Text as T
postCaptionR :: ImagesId ->Handler RepHtml
postCaptionR id = do
    ((result, widget), enctype) <- runFormPost captionForm
    case result of
        FormSuccess cap -> do 
                        runDB (update id [ImagesCaption =. T.unpack cap])
                        redirect RedirectTemporary (ImageR id)
        _ ->  redirect RedirectTemporary $ ImageR id
    

