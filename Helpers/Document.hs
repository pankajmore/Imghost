{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Helpers.Document 
    ( getName
    , getThumb
    , getExtension
    , staticUpload
    ) where
import Yesod
import Data.Text (Text)
import Yesod.Static
import qualified Data.Text as T
getName :: Text ->  Text
getName = T.takeWhile (/='.')

getThumb :: Text -> Text
getThumb iName = T.concat [getName iName, "-thumb", getExtension iName]

getExtension :: Text ->  Text
getExtension = T.dropWhile (/='.')

staticUpload :: Text -> StaticRoute 
staticUpload x = StaticRoute ["upload",x] []   

