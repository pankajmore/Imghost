{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Search (getSearchR) where
import Foundation
import Forms.Search
import Data.Time
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import Control.Applicative
import qualified Data.Text as T
getSearchR :: Int -> Handler RepHtml
getSearchR pageNumber = do
    ((result, widget), enctype) <- runFormGet searchForm
    case result of
        FormSuccess tags -> do 
                        let tagquery = T.unpack tags
                        let resultsPerPage = 5
                        imgList <- map (imagesImageName . snd) <$> runDB (selectList [ ImagesImageTag ==. tagquery
                                ]
                                [ Desc ImagesCreated
                                , LimitTo resultsPerPage
                                , OffsetBy $ (pageNumber - 1) * resultsPerPage
                                ])
                        defaultLayout $(widgetFile "search")
        _ -> defaultLayout [whamlet|
<p>Invalid input failed, let's try again.
|]

