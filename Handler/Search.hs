{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Search (postSearchR) where
import Foundation
import Forms.Search
import Data.Time
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import qualified Data.Text as T
postSearchR :: Handler RepHtml
postSearchR = do
    ((result, widget), enctype) <- runFormPost searchForm
    case result of
        FormSuccess tags -> do 
                        defaultLayout $(widgetFile "search")
        _ -> defaultLayout [whamlet|
<p>Invalid input, let's try again.
|]

