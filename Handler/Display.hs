{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Display (getDisplayR) where
import Foundation
import Data.Time
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import qualified Data.Text as T
import Helpers.Storage 
import Helpers.Document
getDisplayR :: Text -> Int -> Handler RepHtml
getDisplayR tagquery n = do
                        let count = 100
                        imgList <- getImageByTag (Just tagquery) (Just count) ((n-1)*count) 
                        allList <- getImageByTag (Just tagquery) Nothing 0
                        let numberOfItems = length allList
                        let pages = [1..((numberOfItems `div` count) + 1)]
                        defaultLayout $ do 
                            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
                            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.16/jquery-ui.js"
                            addScript $ StaticR js_jquery_image_gallery_js
                            addScript $ StaticR js_gallery_init_js
                            addStylesheetRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.13/themes/south-street/jquery-ui.css"
                            addStylesheet $ StaticR css_jquery_image_gallery_css
                            addStylesheet $ StaticR css_gallery_style_css
                            $(widgetFile "display")


