{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Json (getJsonR) where
import Foundation
import Forms.Upload
import Data.Time
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import qualified Data.Text as T
import System.Cmd (system)
import Data.Text (Text)
import Control.Applicative 
import Data.Aeson

getJsonR :: Handler RepJson
getJsonR = do 
    m <- getYesod
    mSearch <- lookupGetParam "search"
    mCount <- lookupGetParam "count"
    mOffset <- lookupGetParam "offset"
    let offset = maybe 0 (read . T.unpack) mOffset
    let search = maybe [] (\x->[ ImagesImageTag ==. (T.unpack x)]) mSearch  -- change Query String To Text instead of String 
    let count = maybe 20 (read . T.unpack) mCount
    imgList <- map (getQuad m) <$> runDB (selectList 
        search 
        [ Desc ImagesCreated
        , LimitTo count 
        , OffsetBy offset
        ])
    jsonToRepJson.toJSON $ map convert imgList
 where 
    convert (name,tag,caption,lnk) = JsonImage (sUploadDirectory ++ name) caption tag lnk
    getQuad m x = (imagesImageName $ snd x,imagesImageTag $ snd x,imagesCaption $ snd x, yesodRender m (ImageR $ fst x) [])

