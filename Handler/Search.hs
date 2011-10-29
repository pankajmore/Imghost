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
getSearchR :: Handler RepHtml
getSearchR = do
    ((result, widget), enctype) <- runFormGet searchForm
    case result of
        FormSuccess tags -> do 
                        let tagquery = T.unpack tags
                        redirect RedirectTemporary $ DisplayR tagquery 1
        _ -> do setMessage "SearchForm Error"
                redirect RedirectTemporary RootR

