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
        addStylesheet $ StaticR css_jquery_fileupload_ui_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.16/jquery-ui.js"
        addScript $ StaticR js_jquery_tmpl_js
        addScript $ StaticR js_jquery_cookie_js
        addScript $ StaticR js_jquery_image_gallery_js
        addScript $ StaticR js_jquery_iframe_transport_js
        addScript $ StaticR js_jquery_fileupload_js
        addScript $ StaticR js_jquery_fileupload_ui_js
        addScript $ StaticR js_jquery_fileupload_uix_js
        addScript $ StaticR js_en_js
        addScript $ StaticR js_chosen_jquery_js
        addScript $ StaticR js_slider_js
        addScript $ StaticR js_demo_js
        addScript $ StaticR js_application_bak_js
        --toWidget [julius|
        --jQuery(function($){
        -- $(function(){new Application({"max_file_size":5000000,"authenticity_token":{"name":"request_authenticity_token","value":"wdkXQKZ1i-l0EnKVca8URDZpTBFKoJQM"}},locale)});});
        -- |]
        $(widgetFile "homepage")
