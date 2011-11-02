{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.SendJSON (getSendJSONR) where
import Foundation
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import Control.Applicative
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Blaze.ByteString.Builder as BB
import Data.Aeson
getSendJSONR :: Int -> Handler RepJson 
getSendJSONR len= do
    imgList <- map (getTriple . snd) <$> runDB (selectList [] [Desc ImagesCreated, LimitTo len])
    jsonToRepJson.toJSON $ map convert imgList
 where 
    convert (name,tag,caption) = JsonImage (sUploadDirectory ++ name) caption tag
    getTriple x = (imagesImageName x,imagesImageTag x,imagesCaption x)
