{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Forms.Caption (captionForm) where

import Foundation
import Yesod 
import Data.Text (Text)
import Control.Applicative

captionForm :: Html -> Form ImgHost ImgHost (FormResult Text, Widget)
captionForm extra = do 
    (caption, icaption) <- mreq textField "Caption " $ Just "Edit Caption"
    return (caption , [whamlet|
#{extra}
    ^{fvInput icaption}
    <input class="caption" type=Submit value="Submit" alt="Submit">

|])
                
