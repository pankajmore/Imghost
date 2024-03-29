{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Display (getDisplayR) where
import Foundation
import Data.Time
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import qualified Data.Text as T
import Helpers.Document
getDisplayR :: [Char] -> Int -> Handler RepHtml
getDisplayR tagquery pageNumber = do
                        let resultsPerPage = 2
                        let n = pageNumber
                        imgList <- map getPair <$> runDB (selectList [ ImagesImageTag ==. tagquery
                                ]
                                [ Desc ImagesCreated
                                , LimitTo resultsPerPage
                                , OffsetBy $ (pageNumber - 1) * resultsPerPage
                                ])
                        items <- runDB $ selectList [ ImagesImageTag ==. tagquery] []
                        let numberofitems = length items
                        let pages = [1..((numberofitems `div` resultsPerPage) + 1)]
                        defaultLayout $(widgetFile "display")
        where getPair i = (fst i , imagesImageName $ snd i) 


