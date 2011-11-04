{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Helpers.Document where
import Foundation 
import qualified Data.Text as T
getName :: Text ->  Text
getName = T.takeWhile (/='.')

getThumb :: Text -> Text
getThumb iName = T.replace "." "-thumb." iName


getExtension :: Text ->  Text
getExtension = T.dropWhile (/='.')

staticUpload :: Text -> StaticRoute 
staticUpload x = StaticRoute ["upload",x] [("","")]   

