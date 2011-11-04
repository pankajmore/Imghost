{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Delete (postDeleteImageR) where
import Foundation
import Forms.Image
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import Helpers.Storage 
import Control.Applicative
import qualified Data.Text as T
postDeleteImageR :: SqlImageId ->Handler RepHtml
postDeleteImageR id = do
    ((iresult, iwidget), ienctype) <- runFormPost (imageForm images_delete_png)
    case iresult of
        FormSuccess _ -> do 
                        ownerid <- getOwner id
                        requireAdmin ownerid
                        deleteImage id
                        redirect RedirectTemporary RootR
        _ ->  redirect RedirectTemporary $ ImageR id
    

