{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Models where

import Yesod 
import Database.Persist.Sqlite
import Data.Text(Text)
import Data.Time
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Images
    imageName String
    imageTag String
    caption String
    votes Int
    created UTCTime
    ImageName imageName
|]
data Img = Img
    { img :: FileInfo
    , tags :: Text
    }deriving Show

