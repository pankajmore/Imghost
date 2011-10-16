{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Root (getRootR) where
import Yesod 
--import Helpers.Documents
import Data.Text (Text)
import Foundation (uploadForm)
{-import Forms.Upload (uploadForm)-}
getRootR :: Handler RepHtml

getRootR = do
    ((res,widget),enctype) <- generateFormPost uploadForm
    defaultLayout $ do 
        setTitle "Home"
        --addKeywords ["Images","Home"]
        $(widgetFile "homepage")
