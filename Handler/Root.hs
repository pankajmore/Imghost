{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Root (getRootR) where
import Foundation
import Forms.Upload
import Forms.Search
getRootR :: Handler RepHtml
getRootR = do
    ((_,widgetSearch),enctypeSearch) <- generateFormGet searchForm
    ((_,widgetUpload),enctypeUpload) <- runFormPost uploadForm
    defaultLayout $ do 
        setTitle "Home"
        $(widgetFile "homepage")
