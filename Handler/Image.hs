{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Image (getImageR) where
import Foundation
import Forms.Upload
import Data.Time
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import qualified Data.Text as T
import Data.Maybe (fromJust)
getImageR :: ImagesId -> Handler RepHtml
getImageR id = do
                imageName <- getImage id
                case imageName of
                    Just iName ->do let image = sUploadDirectory ++ (iName)
                                    defaultLayout $(widgetFile "image")
                    Nothing -> do setMessage "ID not found in the database"
                                  redirect RedirectTemporary RootR


