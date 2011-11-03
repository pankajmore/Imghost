{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Search (getSearchR) where
import Foundation
import Forms.Search
getSearchR :: Handler RepHtml
getSearchR = do
    ((result, widget), enctype) <- runFormGet searchForm
    case result of
        FormSuccess tag -> redirect RedirectTemporary $ DisplayR tag 1
        _ -> do setMessage "SearchForm Error"
                redirect RedirectTemporary RootR

