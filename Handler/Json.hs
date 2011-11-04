{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Json (getJsonR) where
import Foundation
import Helpers.Document
import Helpers.Storage
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson
import qualified Data.Text.Read as TR
getJsonR :: Handler RepJson
getJsonR = do 
    m <- getYesod
    mSearch <- lookupGetParam "search"
    mCount <- lookupGetParam "count"
    mOffset <- lookupGetParam "offset"
    let offset = maybe 0 (\x -> either (\_->0) fst (TR.decimal x) ) mOffset  -- can not fail 
    let count = maybe 20  (\x -> either (\_->20) fst (TR.decimal x) ) mCount
    imgList <- getImageByTag mSearch count offset
    jsonToRepJson.toJSON $ map (\(a,b) -> toJsonImage (yesodRender m (ImageR a) [] ) b) imgList

