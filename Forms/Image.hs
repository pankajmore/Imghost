{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Forms.Image (imageForm) where

import Foundation
import Yesod 
import Control.Applicative
imageForm :: String -> Html -> Form ImgHost ImgHost (FormResult (), Widget)
imageForm src extra = return ( FormSuccess () , [whamlet|
#{extra}
        <input type=image src=#{src} value="Submit" alt="Submit" width="20" height="20">
|])

