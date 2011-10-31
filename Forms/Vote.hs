{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Forms.Vote (voteIForm,voteDForm) where

import Foundation
import Yesod 
import Control.Applicative
voteIForm :: Html -> Form ImgHost ImgHost (FormResult (), Widget)
voteIForm extra = return ( FormSuccess () , [whamlet|
#{extra}
        <input type=image src="/static/images/thumbsup.jpg" value="Submit" alt="Submit" width="20" height="20">
|])
voteDForm :: Html -> Form ImgHost ImgHost (FormResult (), Widget)
voteDForm extra = return ( FormSuccess () , [whamlet|
#{extra}
        <input type=image src="/static/images/thumbsdown.jpg" value="Submit" alt="Submit" width="20" height="20">
|])

