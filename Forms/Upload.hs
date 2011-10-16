{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Forms.Upload (uploadForm) where

import Yesod
import Data.Text (Text)
import Control.Applicative
import Models

uploadForm :: Html -> Form ImgHost ImgHost (FormResult Img, Widget)
uploadForm = renderTable $ Img
    <$> areq textField "Name" Nothing
    <*> areq textField "Tags" Nothing
