{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Models(Img(..)) where

import Yesod 
import Data.Text(Text)
data Img = Img
    { img :: FileInfo 
    , tags :: Text
    }deriving Show

