{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Forms.Upload (uploadForm) where

import Foundation
import Yesod 
import Data.Text (Text)
import Control.Applicative

uploadForm :: Html -> Form ImgHost ImgHost (FormResult Img, Widget)
uploadForm = renderTable $ Img
    <$> fileAFormReq "Upload Image"
    <*> areq (selectField defaultTags) "Tags" Nothing


