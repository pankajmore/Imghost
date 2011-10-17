{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Root (getRootR) where
import Foundation
import Forms.Upload
getRootR :: Handler RepHtml
getRootR = do
    ((_,widget),enctype) <- runFormPost uploadForm
    defaultLayout $ do 
        setTitle "Home"
        $(widgetFile "homepage")
