{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Search (postSearchR) where
import Foundation
import Forms.Search
import Data.Time
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import Control.Applicative
import qualified Data.Text as T
postSearchR :: Handler RepHtml
postSearchR = do
    ((result, widget), enctype) <- runFormPost searchForm
    case result of
        FormSuccess t -> do 
                        let tags =  tag t
                        let tagquery = T.unpack tags
                        let resultsPerPage = 5
                        let pageNumber = 1
                        imgList <- map (imagesImageName . snd) <$> runDB (selectList [ ImagesImageTag ==. tagquery
                                ]
                                [ Desc ImagesCreated
                                , LimitTo resultsPerPage
                                , OffsetBy $ (pageNumber - 1) * resultsPerPage
                                ])
                        defaultLayout $(widgetFile "search")
        _ -> defaultLayout [whamlet|
<p>Invalid input, let's try again.
|]

