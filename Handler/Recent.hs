{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Recent (getRecentR) where
import Foundation
getRecentR :: Handler RepHtml
getRecentR = defaultLayout $ do 
                                setTitle "Recent Images"
                                addStylesheet $ StaticR css_slider_min_css
                                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
                                addScript $ StaticR js_slider_js
                                addScript $ StaticR js_demo_js
                                $(widgetFile "recent")
