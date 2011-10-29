{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Forms.Search (searchForm) where

import Foundation
import Yesod 
import Data.Text (Text)
import Control.Applicative

searchForm :: Html -> Form ImgHost ImgHost (FormResult Text, Widget)
searchForm = renderTable $ areq (selectField defaultTags) "Tag " Nothing
