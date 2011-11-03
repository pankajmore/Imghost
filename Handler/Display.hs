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
                        let count = 2
                        imgList <- getImageByTag tagquery count ((n-1)*count) 
                        let numberOfItems = length imgList
                        let pages = [1..((numberOfItems `div` resultsPerPage) + 1)]
                        defaultLayout $(widgetFile "display")


