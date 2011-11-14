{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Forms.Tag (tagForm) where

import Foundation
import Yesod 
import Data.Text (Text)
import Control.Applicative

tagForm :: [Text] -> Html -> Form ImgHost ImgHost (FormResult [Text], Widget)
tagForm selectedTags extra = do 
    (tag,itag) <- mreq (multiSelectField defaultTags) (FieldSettings ("Tag " :: Text) Nothing (Just "uploadFormInput") Nothing) (Just selectedTags)
    return (tag , [whamlet|
#{extra}
    ^{fvInput itag}
    <input class="tags" type=Submit value="Add Tags" alt="Add Tags">

|])
                
