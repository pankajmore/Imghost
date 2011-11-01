{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Delete (postDeleteImageR) where
import Foundation
import Forms.Image
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import Control.Applicative
import qualified Data.Text as T
postDeleteImageR :: ImagesId ->Handler RepHtml
postDeleteImageR id = do
    ((iresult, iwidget), ienctype) <- runFormPost (imageForm "/static/images/delete.png")
    case iresult of
        FormSuccess _ -> do 
                        deleteImage id
                        redirect RedirectTemporary RootR
        _ ->  redirect RedirectTemporary $ ImageR id
    
