{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Forms.Vote (voteIForm,voteDForm) where

import Foundation
import Yesod 
import Control.Applicative
voteIForm :: Html -> Form ImgHost ImgHost (FormResult (), Widget)
voteIForm = renderTable $ id <$> pure ()
voteDForm :: Html -> Form ImgHost ImgHost (FormResult (), Widget)
voteDForm = renderTable $ id <$> pure ()
