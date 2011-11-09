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
        addStylesheet $ StaticR css_slider_min_css
        addStylesheet $ StaticR css_chosen_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
        addScript $ StaticR js_chosen_jquery_js
        addScript $ StaticR js_slider_js
        addScript $ StaticR js_demo_js
        $(widgetFile "homepage")
