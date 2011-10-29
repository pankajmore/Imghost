{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Recent (getRecentR) where
import Foundation
import Forms.Upload
import Models
import Control.Applicative

getRecentR :: Handler RepHtml
getRecentR = do
                imgList <- map getPair <$> runDB (selectList [] [Desc ImagesCreated, LimitTo 10])
                defaultLayout $ do 
                                setTitle "Recent Images"
                                $(widgetFile "recent")
        where getPair i = (fst i , imagesImageName $ snd i) 
