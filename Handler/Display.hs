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
                        let count = 5
                        imgList <- getImageByTag (Just tagquery) (Just count) ((n-1)*count) 
                        allList <- getImageByTag (Just tagquery) Nothing 0
                        let numberOfItems = length allList
                        let pages = [1..((numberOfItems `div` count) + 1)]
                        defaultLayout $(widgetFile "display")


